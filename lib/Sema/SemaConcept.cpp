//===-- SemaConcept.cpp - Semantic Analysis for Constraints and Concepts --===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file implements semantic analysis for C++ constraints and concepts.
//
//===----------------------------------------------------------------------===//

#include "clang/Sema/TemplateDeduction.h"
#include "clang/Sema/Template.h"
#include "clang/Sema/SemaInternal.h"
#include "clang/AST/ExprCXX.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/PointerUnion.h"
using namespace clang;
using namespace sema;

bool Sema::CheckConstraintExpression(Expr *ConstraintExpression) {
  // C++2a [temp.constr.atomic]p1
  // ..E shall be a constant expression of type bool.

  if (BinaryOperator* BinOp = dyn_cast<BinaryOperator>(ConstraintExpression)) {
    if (BinOp->getOpcode() == BO_LAnd || BinOp->getOpcode() == BO_LOr) {
      return CheckConstraintExpression(BinOp->getLHS())
        && CheckConstraintExpression(BinOp->getRHS());
    }
  }
  // An atomic constraint!
  if (!ConstraintExpression->isTypeDependent()) {
    if (!Context.hasSameType(ConstraintExpression->getType()
                                .getNonReferenceType().getUnqualifiedType(),
                             Context.BoolTy)) {
      Diag(ConstraintExpression->getExprLoc(),
           diag::err_non_bool_atomic_constraint)
              << ConstraintExpression << ConstraintExpression->getType();
      return false;
    } else if (ImplicitCastExpr *E =
                             dyn_cast<ImplicitCastExpr>(ConstraintExpression)) {
      // This will catch expressions such as '2 && true'
      return CheckConstraintExpression(E->getSubExpr());
    }
  }
  return true;
}

bool Sema::CheckRedeclarationConstraintMatch(const Expr *OldAC,
                                             const Expr *NewAC) {
  if (!(NewAC || OldAC))
    return true; // Nothing to check; no mismatch.
  if (NewAC && OldAC) {
    llvm::FoldingSetNodeID OldACInfo, NewACInfo;
    NewAC->Profile(NewACInfo, Context, /*Canonical=*/true);
    OldAC->Profile(OldACInfo, Context, /*Canonical=*/true);
    if (NewACInfo == OldACInfo)
      return true; // All good; no mismatch.
  }
  return false;
}

template<typename TemplateDeclT>
static bool
calculateConstraintSatisfaction(Sema& S, TemplateDeclT *Template,
                                ArrayRef<TemplateArgument> TemplateArgs,
                                SourceLocation TemplateNameLoc,
                                MultiLevelTemplateArgumentList &MLTAL,
                                const Expr *ConstraintExpr,
                                ConstraintSatisfaction &Satisfaction) {
  if (auto *BO = dyn_cast<BinaryOperator>(ConstraintExpr)) {
    if (BO->getOpcode() == BO_LAnd || BO->getOpcode() == BO_LOr) {
      if (calculateConstraintSatisfaction(S, Template, TemplateArgs,
                                          TemplateNameLoc, MLTAL, BO->getLHS(),
                                          Satisfaction))
        return true;

      bool IsLHSSatisfied = Satisfaction.IsSatisfied;

      if (IsLHSSatisfied && BO->getOpcode() == BO_LOr)
        // This disjunction is satisfied - diagnostic information will not be
        // needed for RHS, no need to generate it.
        return false;

      if (calculateConstraintSatisfaction(S, Template, TemplateArgs,
                                          TemplateNameLoc, MLTAL, BO->getRHS(),
                                          Satisfaction))
        return true;

      if (BO->getOpcode() == BO_LAnd)
        Satisfaction.IsSatisfied &= IsLHSSatisfied;
      else
        Satisfaction.IsSatisfied |= IsLHSSatisfied;

      return false;
    }
  } else if (auto *PO = dyn_cast<ParenExpr>(ConstraintExpr))
    return calculateConstraintSatisfaction(S, Template, TemplateArgs,
                                        TemplateNameLoc, MLTAL,
                                        PO->getSubExpr(), Satisfaction);

  EnterExpressionEvaluationContext ConstantEvaluated(
          S, Sema::ExpressionEvaluationContext::ConstantEvaluated);

  // Atomic constraint - substitute arguments and check satisfaction.
  ExprResult SubstitutedExpression;
  {
    TemplateDeductionInfo Info(TemplateNameLoc);
    Sema::InstantiatingTemplate Inst(S, TemplateNameLoc, Template, TemplateArgs,
                                     Info);
    // We do not want error diagnostics escaping here.
    Sema::SFINAETrap Trap(S);
    SubstitutedExpression = S.SubstExpr(const_cast<Expr*>(ConstraintExpr),
                                        MLTAL);
    if (!SubstitutedExpression.isUsable() ||
        SubstitutedExpression.isInvalid() || Trap.hasErrorOccurred()) {
      // C++2a [temp.constr.atomic]p1
      //   ...If substitution results in an invalid type or expression, the
      //   constraint is not satisfied.
      if (Trap.hasErrorOccurred()) {
        PartialDiagnosticAt SubstDiag{ SourceLocation(),
                                       PartialDiagnostic::NullDiagnostic() };
        Info.takeSFINAEDiagnostic(SubstDiag);
        SmallString<128> DiagString;
        DiagString = ": ";
        SubstDiag.second.EmitToString(S.getDiagnostics(), DiagString);
        Satisfaction.Details.emplace_back(
            ConstraintExpr,
            new (S.Context) ConstraintSatisfaction::SubstitutionDiagnostic{
                    SubstDiag.first,
                    std::string(DiagString.begin(), DiagString.end())});
      }
      Satisfaction.IsSatisfied = false;
      return false;
    }
  }

  if (!S.CheckConstraintExpression(SubstitutedExpression.get()))
    return true;

  assert(!SubstitutedExpression.get()->isInstantiationDependent() &&
         "Instantiation dependent constraint expressions should not get here!");

  if (!SubstitutedExpression.get()->EvaluateAsBooleanCondition(Satisfaction.IsSatisfied,
                                                               S.Context)) {
      // C++2a [temp.constr.atomic]p1
      //   ...E shall be a constant expression of type bool.
    S.Diag(SubstitutedExpression.get()->getLocStart(),
           diag::err_non_constant_constraint_expression)
        << SubstitutedExpression.get();
    return true;
  }

  if (!Satisfaction.IsSatisfied)
    Satisfaction.Details.emplace_back(ConstraintExpr,
                                      SubstitutedExpression.get());

  return false;
}

template<typename TemplateDeclT>
static bool CheckConstraintSatisfaction(Sema &S, TemplateDeclT *Template,
                                        const Expr *ConstraintExpr,
                                        ArrayRef<TemplateArgument> TemplateArgs,
                                        SourceLocation TemplateNameLoc,
                                        ConstraintSatisfaction &Satisfaction) {
  if (!ConstraintExpr) {
    Satisfaction.IsSatisfied = true;
    return false;
  }

  for (auto& Arg : TemplateArgs)
    if (Arg.isInstantiationDependent()) {
      // No need to check satisfaction for dependent constraint expressions.
      Satisfaction.IsSatisfied = true;
      return false;
    }

  MultiLevelTemplateArgumentList MLTAL;
  MLTAL.addOuterTemplateArguments(TemplateArgs);

  return calculateConstraintSatisfaction(S, Template, TemplateArgs,
                                         TemplateNameLoc, MLTAL, ConstraintExpr,
                                         Satisfaction);
}

bool Sema::CheckConstraintSatisfaction(TemplateDecl *Template,
                                       const Expr *ConstraintExpr,
                                       ArrayRef<TemplateArgument> TemplateArgs,
                                       SourceLocation TemplateNameLoc,
                                       ConstraintSatisfaction &Satisfaction) {
  return ::CheckConstraintSatisfaction(*this, Template, ConstraintExpr,
                                       TemplateArgs, TemplateNameLoc,
                                       Satisfaction);
}

bool
Sema::CheckConstraintSatisfaction(ClassTemplatePartialSpecializationDecl* Part,
                                  const Expr *ConstraintExpr,
                                  ArrayRef<TemplateArgument> TemplateArgs,
                                  SourceLocation TemplateNameLoc,
                                  ConstraintSatisfaction &Satisfaction) {
  return ::CheckConstraintSatisfaction(*this, Part, ConstraintExpr,
                                       TemplateArgs, TemplateNameLoc,
                                       Satisfaction);
}

bool
Sema::CheckConstraintSatisfaction(VarTemplatePartialSpecializationDecl* Partial,
                                  const Expr *ConstraintExpr,
                                  ArrayRef<TemplateArgument> TemplateArgs,
                                  SourceLocation TemplateNameLoc,
                                  ConstraintSatisfaction &Satisfaction) {
  return ::CheckConstraintSatisfaction(*this, Partial, ConstraintExpr,
                                       TemplateArgs, TemplateNameLoc,
                                       Satisfaction);
}

bool Sema::EnsureTemplateArgumentListConstraints(
    TemplateDecl *TD, ArrayRef<TemplateArgument> TemplateArgs,
    SourceLocation TemplateNameLoc) {
  ConstraintSatisfaction Satisfaction;
  if (CheckConstraintSatisfaction(TD, TD->getAssociatedConstraints(),
                                  TemplateArgs, TemplateNameLoc, Satisfaction))
    return true;

  if (!Satisfaction.IsSatisfied) {
    SmallString<128> TemplateArgString;
    TemplateArgString = " ";
    TemplateArgString += getTemplateArgumentBindingsText(
        TD->getTemplateParameters(), TemplateArgs.data(), TemplateArgs.size());

    Diag(TemplateNameLoc, diag::err_template_arg_list_constraints_not_satisfied)
        << (int)getTemplateNameKindForDiagnostics(TemplateName(TD)) << TD
        << TemplateArgString;
    DiagnoseUnsatisfiedConstraint(Satisfaction);
    return true;
  }
  return false;
}

static void diagnoseWellFormedUnsatisfiedConstraintExpr(Sema &S,
                                                        Expr *SubstExpr,
                                                        bool First = true) {
  if (BinaryOperator *BO = dyn_cast<BinaryOperator>(SubstExpr)) {
    switch (BO->getOpcode()) {
    // These two cases will in practice only be reached when using fold
    // expressions with || and &&, since otherwise the || and && will have been
    // broken down into atomic constraints during satisfaction checking.
    case BO_LOr:
      // Or evaluated to false - meaning both RHS and LHS evaluated to false.
      diagnoseWellFormedUnsatisfiedConstraintExpr(S, BO->getLHS(), First);
      diagnoseWellFormedUnsatisfiedConstraintExpr(S, BO->getRHS(),
                                                  /*First=*/false);
      return;
    case BO_LAnd:
      bool LHSSatisfied;
      BO->getLHS()->EvaluateAsBooleanCondition(LHSSatisfied, S.Context);
      if (LHSSatisfied) {
        // LHS is true, so RHS must be false.
        diagnoseWellFormedUnsatisfiedConstraintExpr(S, BO->getRHS(), First);
        return;
      }
      // LHS is false
      diagnoseWellFormedUnsatisfiedConstraintExpr(S, BO->getLHS(), First);

      // RHS might also be false
      bool RHSSatisfied;
      BO->getRHS()->EvaluateAsBooleanCondition(RHSSatisfied, S.Context);
      if (!RHSSatisfied)
        diagnoseWellFormedUnsatisfiedConstraintExpr(S, BO->getRHS(),
                                                    /*First=*/false);
      return;
    case BO_GE:
    case BO_LE:
    case BO_GT:
    case BO_LT:
    case BO_EQ:
    case BO_NE:
      if (BO->getLHS()->getType()->isIntegerType() &&
          BO->getRHS()->getType()->isIntegerType()) {
        llvm::APSInt SimplifiedLHS;
        llvm::APSInt SimplifiedRHS;
        BO->getLHS()->EvaluateAsInt(SimplifiedLHS, S.Context);
        BO->getRHS()->EvaluateAsInt(SimplifiedRHS, S.Context);
        S.Diag(SubstExpr->getSourceRange().getBegin(),
               diag::note_atomic_constraint_evaluated_to_false_elaborated)
            << (int)First << SubstExpr << SimplifiedLHS.toString(10)
            << BinaryOperator::getOpcodeStr(BO->getOpcode())
            << SimplifiedRHS.toString(10);
        return;
      }
      break;

    default:
      break;
    }
  } else if (ParenExpr *PE = dyn_cast<ParenExpr>(SubstExpr)) {
    diagnoseWellFormedUnsatisfiedConstraintExpr(S, PE->getSubExpr(), First);
    return;
  } else if (auto *CSE = dyn_cast<ConceptSpecializationExpr>(SubstExpr)) {
    if (CSE->getTemplateArgumentListInfo()->NumTemplateArgs == 1) {
      S.Diag(
          CSE->getSourceRange().getBegin(),
          diag::
          note_single_arg_concept_specialization_constraint_evaluated_to_false)
          << (int)First
          << CSE->getTemplateArgumentListInfo()->arguments()[0].getArgument()
          << CSE->getNamedConcept();
    } else {
      S.Diag(SubstExpr->getSourceRange().getBegin(),
             diag::note_concept_specialization_constraint_evaluated_to_false)
          << (int)First << CSE;
    }
    S.DiagnoseUnsatisfiedConstraint(CSE->getSatisfaction());
    return;
  }

  S.Diag(SubstExpr->getSourceRange().getBegin(),
         diag::note_atomic_constraint_evaluated_to_false)
      << (int)First << SubstExpr;
}

static void diagnoseUnsatisfiedConstraintExpr(
    Sema &S, const Expr *E, ConstraintSatisfaction::Detail Detail,
    bool First = true) {
  if (auto *Diag =
          Detail.dyn_cast<ConstraintSatisfaction::SubstitutionDiagnostic *>()) {
    S.Diag(Diag->first, diag::note_substituted_constraint_expr_is_ill_formed)
        << Diag->second;
    return;
  }

  diagnoseWellFormedUnsatisfiedConstraintExpr(S, Detail.get<Expr *>(), First);
}

void Sema::DiagnoseUnsatisfiedConstraint(
    const ConstraintSatisfaction& Satisfaction) {
  assert(!Satisfaction.IsSatisfied &&
         "Attempted to diagnose a satisfied constraint");
  bool First = true;
  for (auto &Pair : Satisfaction.Details) {
    diagnoseUnsatisfiedConstraintExpr(*this, Pair.first, Pair.second, First);
    First = false;
  }
}
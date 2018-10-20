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

#include "clang/Sema/Sema.h"
#include "clang/Sema/SemaInternal.h"
#include "clang/Sema/SemaDiagnostic.h"
#include "clang/Sema/TemplateDeduction.h"
#include "clang/Sema/Template.h"
#include "clang/AST/ExprCXX.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/PointerUnion.h"
using namespace clang;
using namespace sema;

bool Sema::CheckConstraintExpression(Expr *ConstraintExpression) {
  // C++2a [temp.constr.atomic]p1
  // ..E shall be a constant expression of type bool.

  if (auto *BinOp = dyn_cast<BinaryOperator>(ConstraintExpression))
    if (BinOp->getOpcode() == BO_LAnd || BinOp->getOpcode() == BO_LOr)
      return CheckConstraintExpression(BinOp->getLHS()) &&
          CheckConstraintExpression(BinOp->getRHS());

  // An atomic constraint!
  if (ConstraintExpression->isTypeDependent())
    return true;

  QualType Type = ConstraintExpression->IgnoreParenImpCasts()->getType()
                      .getNonReferenceType().getUnqualifiedType();
  if (!Context.hasSameType(Type, Context.BoolTy)) {
    Diag(ConstraintExpression->getExprLoc(),
         diag::err_non_bool_atomic_constraint) << Type
        << ConstraintExpression->getSourceRange();
    return false;
  }
  return true;
}

bool Sema::CheckRedeclarationConstraintMatch(ArrayRef<const Expr *> OldAC,
                                             ArrayRef<const Expr *> NewAC) {
  if (NewAC.empty() && OldAC.empty())
    return true; // Nothing to check; no mismatch.
  if (NewAC.size() != OldAC.size())
    return false;
  llvm::FoldingSetNodeID OldACInfo, NewACInfo;
  for (const Expr *E : NewAC)
    E->Profile(NewACInfo, Context, /*Canonical=*/true);
  for (const Expr *E : OldAC)
    E->Profile(OldACInfo, Context, /*Canonical=*/true);
  return NewACInfo == OldACInfo;
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

      if (BO->getOpcode() == BO_LOr && IsLHSSatisfied)
        // [temp.constr.op] p3
        //    A disjunction is a constraint taking two operands. To determine if
        //    a disjunction is satisfied, the satisfaction of the first operand
        //    is checked. If that is satisfied, the disjunction is satisfied.
        //    Otherwise, the disjunction is satisfied if and only if the second
        //    operand is satisfied.
        return false;

      if (BO->getOpcode() == BO_LAnd && !IsLHSSatisfied)
        // [temp.constr.op] p2
        //    A conjunction is a constraint taking two operands. To determine if
        //    a conjunction is satisfied, the satisfaction of the first operand
        //    is checked. If that is not satisfied, the conjunction is not
        //    satisfied. Otherwise, the conjunction is satisfied if and only if
        //    the second operand is satisfied.
        return false;

      return calculateConstraintSatisfaction(S, Template, TemplateArgs,
                                             TemplateNameLoc, MLTAL,
                                             BO->getRHS(), Satisfaction);
    }
  } else if (auto *PO = dyn_cast<ParenExpr>(ConstraintExpr))
    return calculateConstraintSatisfaction(S, Template, TemplateArgs,
                                           TemplateNameLoc, MLTAL,
                                           PO->getSubExpr(), Satisfaction);
  else if (auto *C = dyn_cast<ExprWithCleanups>(ConstraintExpr))
    return calculateConstraintSatisfaction(S, Template, TemplateArgs,
                                           TemplateNameLoc, MLTAL,
                                           PO->getSubExpr(), Satisfaction);

  EnterExpressionEvaluationContext ConstantEvaluated(
          S, Sema::ExpressionEvaluationContext::ConstantEvaluated);

  // Atomic constraint - substitute arguments and check satisfaction.
  ExprResult SubstitutedExpression;
  {
    TemplateDeductionInfo Info(TemplateNameLoc);
    Sema::InstantiatingTemplate Inst(S, ConstraintExpr->getLocStart(),
        Sema::InstantiatingTemplate::ConstraintSubstitution{}, Template, Info,
        ConstraintExpr->getSourceRange());
    if (Inst.isInvalid())
      return true;
    // We do not want error diagnostics escaping here.
    Sema::SFINAETrap Trap(S);
    SubstitutedExpression = S.SubstExpr(const_cast<Expr*>(ConstraintExpr),
                                        MLTAL);
    if (SubstitutedExpression.isInvalid()) {
      // C++2a [temp.constr.atomic]p1
      //   ...If substitution results in an invalid type or expression, the
      //   constraint is not satisfied.
      if (!Trap.hasErrorOccurred())
        // A non-SFINAE error has occured as a result of this substitution.
        return true;

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
      Satisfaction.IsSatisfied = false;
      return false;
    }
  }

  if (!S.CheckConstraintExpression(SubstitutedExpression.get()))
    return true;

  SmallVector<PartialDiagnosticAt, 2> EvaluationDiags;
  Expr::EvalResult EvalResult;
  EvalResult.Diag = &EvaluationDiags;
  if (!SubstitutedExpression.get()->EvaluateAsRValue(EvalResult, S.Context)) {
      // C++2a [temp.constr.atomic]p1
      //   ...E shall be a constant expression of type bool.
    S.Diag(SubstitutedExpression.get()->getLocStart(),
           diag::err_non_constant_constraint_expression)
        << SubstitutedExpression.get()->getSourceRange();
    for (const PartialDiagnosticAt &PDiag : EvaluationDiags)
      S.Diag(PDiag.first, PDiag.second);
    return true;
  }

  Satisfaction.IsSatisfied = EvalResult.Val.getInt().getBoolValue();
  if (!Satisfaction.IsSatisfied)
    Satisfaction.Details.emplace_back(ConstraintExpr,
                                      SubstitutedExpression.get());

  return false;
}

template<typename TemplateDeclT>
static bool CheckConstraintSatisfaction(Sema &S, TemplateDeclT *Template,
                                        ArrayRef<const Expr *> ConstraintExprs,
                                        ArrayRef<TemplateArgument> TemplateArgs,
                                        SourceRange TemplateIDRange,
                                        ConstraintSatisfaction &Satisfaction) {
  if (ConstraintExprs.empty()) {
    Satisfaction.IsSatisfied = true;
    return false;
  }

  for (auto& Arg : TemplateArgs)
    if (Arg.isInstantiationDependent()) {
      // No need to check satisfaction for dependent constraint expressions.
      Satisfaction.IsSatisfied = true;
      return false;
    }

  Sema::InstantiatingTemplate Inst(S, TemplateIDRange.getBegin(),
      Sema::InstantiatingTemplate::ConstraintsCheck{}, Template, TemplateArgs,
      TemplateIDRange);
  if (Inst.isInvalid())
    return true;

  MultiLevelTemplateArgumentList MLTAL;
  MLTAL.addOuterTemplateArguments(TemplateArgs);

  for (const Expr *ConstraintExpr : ConstraintExprs) {
    if (calculateConstraintSatisfaction(S, Template, TemplateArgs,
                                        TemplateIDRange.getBegin(), MLTAL,
                                        ConstraintExpr, Satisfaction))
      return true;
    if (!Satisfaction.IsSatisfied)
      // [temp.constr.op] p2
      //   [...] To determine if a conjunction is satisfied, the satisfaction
      //   of the first operand is checked. If that is not satisfied, the
      //   conjunction is not satisfied. [...]
      return false;
  }
  return false;
}

bool Sema::CheckConstraintSatisfaction(TemplateDecl *Template,
                                       ArrayRef<const Expr *> ConstraintExprs,
                                       ArrayRef<TemplateArgument> TemplateArgs,
                                       SourceRange TemplateIDRange,
                                       ConstraintSatisfaction &Satisfaction) {
  return ::CheckConstraintSatisfaction(*this, Template, ConstraintExprs,
                                       TemplateArgs, TemplateIDRange,
                                       Satisfaction);
}

bool
Sema::CheckConstraintSatisfaction(ClassTemplatePartialSpecializationDecl* Part,
                                  ArrayRef<const Expr *> ConstraintExprs,
                                  ArrayRef<TemplateArgument> TemplateArgs,
                                  SourceRange TemplateIDRange,
                                  ConstraintSatisfaction &Satisfaction) {
  return ::CheckConstraintSatisfaction(*this, Part, ConstraintExprs,
                                       TemplateArgs, TemplateIDRange,
                                       Satisfaction);
}

bool
Sema::CheckConstraintSatisfaction(VarTemplatePartialSpecializationDecl* Partial,
                                  ArrayRef<const Expr *> ConstraintExprs,
                                  ArrayRef<TemplateArgument> TemplateArgs,
                                  SourceRange TemplateIDRange,
                                  ConstraintSatisfaction &Satisfaction) {
  return ::CheckConstraintSatisfaction(*this, Partial, ConstraintExprs,
                                       TemplateArgs, TemplateIDRange,
                                       Satisfaction);
}

bool Sema::EnsureTemplateArgumentListConstraints(
    TemplateDecl *TD, ArrayRef<TemplateArgument> TemplateArgs,
    SourceRange TemplateIDRange) {
  ConstraintSatisfaction Satisfaction;
  if (CheckConstraintSatisfaction(TD, TD->getAssociatedConstraints(),
                                  TemplateArgs, TemplateIDRange, Satisfaction))
    return true;

  if (!Satisfaction.IsSatisfied) {
    SmallString<128> TemplateArgString;
    TemplateArgString = " ";
    TemplateArgString += getTemplateArgumentBindingsText(
        TD->getTemplateParameters(), TemplateArgs.data(), TemplateArgs.size());

    Diag(TemplateIDRange.getBegin(),
         diag::err_template_arg_list_constraints_not_satisfied)
        << (int)getTemplateNameKindForDiagnostics(TemplateName(TD)) << TD
        << TemplateArgString << TemplateIDRange;
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
    if (CSE->getTemplateArguments().size() == 1) {
      S.Diag(
          CSE->getSourceRange().getBegin(),
          diag::
          note_single_arg_concept_specialization_constraint_evaluated_to_false)
          << (int)First << CSE->getTemplateArguments()[0]
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
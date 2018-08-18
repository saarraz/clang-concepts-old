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

void
Sema::DiagnoseRedeclarationConstraintMismatch(const TemplateParameterList *Old,
                                              const TemplateParameterList *New){
  Diag(New->getTemplateLoc(),
       diag::err_template_different_associated_constraints);

  Diag(Old->getTemplateLoc(), diag::note_template_prev_declaration)
        << /*declaration*/0;
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
namespace {
struct AtomicConstraint {
  AtomicConstraint(Expr *ConstraintExpr,
      const ASTTemplateArgumentListInfo *ParameterMapping = nullptr) :
      ConstraintExpr{ConstraintExpr}, ParameterMapping{ParameterMapping} {}

  bool subsumes(ASTContext &C, const AtomicConstraint &Other) const {
    // C++ [temp.constr.order] p2
    //   - an atomic constraint A subsumes another atomic constraint B
    //     if and only if the A and B are identical [...]
    //
    // C++ [temp.constr.atomic] p2
    //   Two atomic constraints are identical if they are formed from the
    //   same expression and the targets of the parameter mappings are
    //   equivalent according to the rules for expressions [...]

    // We do not actually substitute the parameter mappings, therefore the
    // constraint expressions are the originals, and comparing them will
    // suffice.
    if (ConstraintExpr != Other.ConstraintExpr)
      return false;

    // Check that the parameter lists are identical
    if ((!ParameterMapping) != (!Other.ParameterMapping))
      return false;
    if (!ParameterMapping)
      return true;
    if (ParameterMapping->NumTemplateArgs !=
        Other.ParameterMapping->NumTemplateArgs)
      return false;

    for (unsigned I = 0, S = ParameterMapping->NumTemplateArgs; I < S; ++I)
      if (!C.getCanonicalTemplateArgument(
                ParameterMapping->arguments()[I].getArgument())
               .structurallyEquals(C.getCanonicalTemplateArgument(
                   Other.ParameterMapping->arguments()[I].getArgument())))
        return false;


    return true;
  }

  Expr *ConstraintExpr;
  const ASTTemplateArgumentListInfo *ParameterMapping;
};

class NormalizedConstraint {
public:
  enum CompoundConstraintKind { CCK_Conjunction, CCK_Disjunction };

private:
  using CompoundConstraint = llvm::PointerIntPair<
      std::pair<NormalizedConstraint, NormalizedConstraint> *, 1,
      CompoundConstraintKind>;

  llvm::PointerUnion<AtomicConstraint *, CompoundConstraint> Constraint;

  NormalizedConstraint(AtomicConstraint *C) : Constraint{C} {};
  NormalizedConstraint(ASTContext &C, NormalizedConstraint LHS,
                       NormalizedConstraint RHS, CompoundConstraintKind Kind)
      : Constraint{CompoundConstraint{
            new std::pair<NormalizedConstraint, NormalizedConstraint>{LHS, RHS},
            Kind}} {};

public:
  CompoundConstraintKind getCompoundKind() const {
    assert(!isAtomic() && "getCompoundKind called on atomic constraint.");
    return Constraint.get<CompoundConstraint>().getInt();
  }

  bool isAtomic() const { return Constraint.is<AtomicConstraint *>(); }

  NormalizedConstraint &getLHS() const {
    assert(!isAtomic() && "getLHS called on atomic constraint.");
    return Constraint.get<CompoundConstraint>().getPointer()->first;
  }

  NormalizedConstraint &getRHS() const {
    assert(!isAtomic() && "getRHS called on atomic constraint.");
    return Constraint.get<CompoundConstraint>().getPointer()->second;
  }

  AtomicConstraint *getAtomicConstraint() const {
    assert(isAtomic() &&
           "getAtomicConstraint called on non-atomic constraint.");
    return Constraint.get<AtomicConstraint *>();
  }
  static llvm::Optional<NormalizedConstraint> fromConstraintExpr(
      Sema &S, Expr *E, TemplateDecl *TD = nullptr,
      const ASTTemplateArgumentListInfo *ParameterMapping = nullptr) {
    assert(E != nullptr);

    // C++ [temp.constr.normal]p1.1
    // - The normal form of an expression (E) is the normal form of E.
    if (ParenExpr *P = dyn_cast<ParenExpr>(E))
      return fromConstraintExpr(S, P->getSubExpr(), TD, ParameterMapping);
    if (BinaryOperator *BO = dyn_cast<BinaryOperator>(E)) {
      if (BO->getOpcode() == BO_LAnd || BO->getOpcode() == BO_LOr) {
        auto LHS = fromConstraintExpr(S, BO->getLHS(), TD, ParameterMapping);
        if (!LHS)
          return llvm::Optional<NormalizedConstraint>{};
        auto RHS = fromConstraintExpr(S, BO->getRHS(), TD, ParameterMapping);
        if (!RHS)
          return llvm::Optional<NormalizedConstraint>{};

        return NormalizedConstraint(
            S.Context, *LHS, *RHS,
            BO->getOpcode() == BO_LAnd ? CCK_Conjunction : CCK_Disjunction);
      }
    } else if (auto *CSE = dyn_cast<ConceptSpecializationExpr>(E)) {
      const ASTTemplateArgumentListInfo *Mapping =
          CSE->getTemplateArgumentListInfo();
      if (!ParameterMapping) {
        llvm::SmallVector<TemplateArgument, 4> TempList;
        bool InstantiationDependent = false;
        TemplateArgumentListInfo TALI(Mapping->LAngleLoc, Mapping->RAngleLoc);
        for (auto &Arg : Mapping->arguments())
          TALI.addArgument(Arg);
        bool Failed = S.CheckTemplateArgumentList(CSE->getNamedConcept(),
            E->getLocStart(), TALI, /*PartialTemplateArgs=*/false, TempList,
            /*UpdateArgsWithConversions=*/false, &InstantiationDependent);
        // The potential failure case here is this:
        //
        // template<typename U>
        // concept C = true;
        //
        // template<typename T>
        // void foo() requires C<T, T> // The immediate constraint expr
        //                             // contains a CSE with incorrect no.
        //                             // of arguments.
        // {}
        // This will be handled when C<T, T> is parsed.
        assert(
            !Failed &&
            "Unmatched arguments in top level concept specialization "
            "expression should've been caught while it was being constructed");

        if (InstantiationDependent)
          // The case is this:
          //
          // template<typename U, typename T>
          // concept C = true;
          //
          // template<typename... Ts>
          // void foo() requires C<Ts...> // The immediate constraint expr
          //                              // contains a CSE whose parameters
          //                              // are not mappable to arguments
          //                              // without concrete values.
          // {}
          //
          // Just treat C<Ts...> as an atomic constraint.
          return NormalizedConstraint{new (S.Context)
                                          AtomicConstraint(E, Mapping)};

        return fromConstraintExpr(S,
                                  CSE->getNamedConcept()->getConstraintExpr(),
                                  CSE->getNamedConcept(), Mapping);
      }

      assert(TD && "ParameterMapping provided without TemplateDecl");

      TemplateArgumentListInfo TALI(ParameterMapping->LAngleLoc,
                                    ParameterMapping->RAngleLoc);
      for (auto &Arg : ParameterMapping->arguments())
        TALI.addArgument(Arg);
      llvm::SmallVector<TemplateArgument, 4> TempList;
      bool InstantiationDependent = false;
      bool Success =
          !S.CheckTemplateArgumentList(TD, ParameterMapping->LAngleLoc,
                                       TALI, /*PartialTemplateArgs=*/false,
                                       TempList,
                                       /*UpdateArgsWithConversions=*/true,
                                       &InstantiationDependent) &&
          !InstantiationDependent;
      assert(Success && "ParameterMapping should have already been cheked "
                        "against template argument list earlier.");

      auto DiagnoseSubstitutionError = [&](unsigned int Diag) {
        std::string TemplateArgString = S.getTemplateArgumentBindingsText(
            TD->getTemplateParameters(), TempList.data(), TempList.size());
        S.Diag(CSE->getLocStart(), Diag) << CSE << TemplateArgString;
      };

      MultiLevelTemplateArgumentList MLTAL;
      MLTAL.addOuterTemplateArguments(TempList);

      ExprResult Result = S.SubstExpr(CSE, MLTAL);
      if (!Result.isUsable() || Result.isInvalid()) {
        // C++ [temp.constr.normal]
        // If any such substitution results in an invalid type or
        // expression, the program is ill-formed; no diagnostic is required.

        // A diagnostic was already emitted from the substitution , but
        // we'll let the user know why it's not SFINAEd from them.
        DiagnoseSubstitutionError(
            diag::note_could_not_normalize_argument_substitution_failed);
        return llvm::Optional<NormalizedConstraint>{};
      }
      Mapping = cast<ConceptSpecializationExpr>(Result.get())
                    ->getTemplateArgumentListInfo();

      TemplateArgumentListInfo SubstTALI(ParameterMapping->LAngleLoc,
                                         ParameterMapping->RAngleLoc);
      for (auto &Arg : ParameterMapping->arguments())
        SubstTALI.addArgument(Arg);
      llvm::SmallVector<TemplateArgument, 4> Converted;
      if (S.CheckTemplateArgumentList(
              CSE->getNamedConcept(), CSE->getLocStart(), SubstTALI,
              /*PartialTemplateArgs=*/false, Converted,
              /*UpdateArgsWithConversions=*/true, &InstantiationDependent)) {
        // The case is this:
        //
        // template<typename T, typename U>
        // concept C1 = true;
        //
        // template<typename... Ts>
        // concept C2 = C1<Ts...>; // After substituting Ts = {T}, the
        //                         // resulting argument list does not match
        //                         // the parameter list.
        //
        // template<typename T>
        // void foo() requires C2<T> {}
        DiagnoseSubstitutionError(
            diag::note_could_not_normalize_unmatched_argument_list_after_subst);
        return llvm::Optional<NormalizedConstraint>{};
      }
      if (InstantiationDependent)
        // The case is this:
        //
        // template<typename T, typename U>
        // concept C1 = true;
        //
        // template<typename... Us>
        // concept C2 = C1<Us...>; // After substituting Us = {Ts}, we cannot
        //                         // match arguments to parameters.
        //
        // template<typename... Ts>
        // void foo() requires C2<T...> {}
        //
        // Treat the CSE as an atomic expression.
        return NormalizedConstraint{new (S.Context)
                                        AtomicConstraint(E, ParameterMapping)};

      return fromConstraintExpr(S, CSE->getNamedConcept()->getConstraintExpr(),
                                CSE->getNamedConcept(), Mapping);
    }
    return NormalizedConstraint{new (S.Context)
                                    AtomicConstraint(E, ParameterMapping)};
  }
};
} // namespace

using NormalForm =
    llvm::SmallVector<llvm::SmallVector<AtomicConstraint *, 2>, 4>;

static NormalForm makeCNF(const NormalizedConstraint &Normalized) {
  if (Normalized.isAtomic())
    return {{Normalized.getAtomicConstraint()}};

  NormalForm LCNF = makeCNF(Normalized.getLHS());
  NormalForm RCNF = makeCNF(Normalized.getRHS());
  if (Normalized.getCompoundKind() == NormalizedConstraint::CCK_Conjunction) {
    LCNF.reserve(LCNF.size() + RCNF.size());
    while (!RCNF.empty())
      LCNF.push_back(std::move(RCNF.pop_back_val()));
    return LCNF;
  }

  // Disjunction
  NormalForm Res;
  Res.reserve(LCNF.size() * RCNF.size());
  for (auto &LDisjunction : LCNF)
    for (auto &RDisjunction : RCNF) {
      NormalForm::value_type Combined;
      Combined.reserve(LDisjunction.size() + RDisjunction.size());
      std::copy(LDisjunction.begin(), LDisjunction.end(),
                std::back_inserter(Combined));
      std::copy(RDisjunction.begin(), RDisjunction.end(),
                std::back_inserter(Combined));
      Res.emplace_back(Combined);
    }
  return Res;
}

static NormalForm makeDNF(const NormalizedConstraint &Normalized) {
  if (Normalized.isAtomic())
    return {{Normalized.getAtomicConstraint()}};

  NormalForm LDNF = makeDNF(Normalized.getLHS());
  NormalForm RDNF = makeDNF(Normalized.getRHS());
  if (Normalized.getCompoundKind() == NormalizedConstraint::CCK_Disjunction) {
    LDNF.reserve(LDNF.size() + RDNF.size());
    while (!RDNF.empty())
      LDNF.push_back(std::move(RDNF.pop_back_val()));
    return LDNF;
  }

  // Conjunction
  NormalForm Res;
  Res.reserve(LDNF.size() * RDNF.size());
  for (auto &LConjunction : LDNF) {
    for (auto &RConjunction : RDNF) {
      NormalForm::value_type Combined;
      Combined.reserve(LConjunction.size() + RConjunction.size());
      std::copy(LConjunction.begin(), LConjunction.end(),
                std::back_inserter(Combined));
      std::copy(RConjunction.begin(), RConjunction.end(),
                std::back_inserter(Combined));
      Res.emplace_back(Combined);
    }
  }
  return Res;
}

static bool subsumes(Sema &S, Expr *P, Expr *Q) {
  // C++ [temp.constr.order] p2
  //   In order to determine if a constraint P subsumes a constraint Q, P is
  //   transformed into disjunctive normal form, and Q is transformed into
  //   conjunctive normal form. [...]
  auto PNormalized = NormalizedConstraint::fromConstraintExpr(S, P);
  if (!PNormalized)
    // Program is ill formed at this point.
    return false;
  const NormalForm PDNF = makeDNF(*PNormalized);

  auto QNormalized = NormalizedConstraint::fromConstraintExpr(S, Q);
  if (!QNormalized)
    // Program is ill formed at this point.
    return false;
  const NormalForm QCNF = makeCNF(*QNormalized);

  // C++ [temp.constr.order] p2
  //   Then, P subsumes Q if and only if, for every disjunctive clause Pi in the
  //   disjunctive normal form of P, Pi subsumes every conjunctive clause Qj in
  //   the conjuctive normal form of Q, where [...]
  for (const auto &Pi : PDNF) {
    for (const auto &Qj : QCNF) {
      // C++ [temp.constr.order] p2
      //   - [...] a disjunctive clause Pi subsumes a conjunctive clause Qj if
      //     and only if there exists an atomic constraint Pia in Pi for which
      //     there exists an atomic constraint, Qjb, in Qj such that Pia
      //     subsumes Qjb.
      bool Found = false;
      for (const AtomicConstraint *Pia : Pi) {
        for (const AtomicConstraint *Qjb : Qj) {
          if (Pia->subsumes(S.Context, *Qjb)) {
            Found = true;
            break;
          }
        }
        if (Found)
          break;
      }
      if (!Found) {
        return false;
      }
    }
  }
  return true;
}

bool Sema::IsMoreConstrained(NamedDecl *D1, Expr *AC1, NamedDecl *D2,
                             Expr *AC2) {
  if (!AC1)
    return AC2 == nullptr;
  if (!AC2)
    // TD1 has associated constraints and TD2 does not.
    return true;

  std::pair<NamedDecl *, NamedDecl *> Key{D1, D2};
  auto CacheEntry = SubsumptionCache.find(Key);
  if (CacheEntry != SubsumptionCache.end()) {
    return CacheEntry->second;
  }
  bool Subsumes = subsumes(*this, AC1, AC2);
  SubsumptionCache.try_emplace(Key, Subsumes);
  return Subsumes;
}
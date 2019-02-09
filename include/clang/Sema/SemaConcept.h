//===--- SemaConcept.h - Concept Utilities ----------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file provides some common utilities for processing constraints
/// and concepts.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_SEMA_SEMACONCEPTS_H
#define LLVM_CLANG_SEMA_SEMACONCEPTS_H
#include "clang/AST/Expr.h"
#include "clang/Basic/SourceLocation.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallVector.h"
#include <string>
#include <utility>
namespace clang {
class ConceptSpecializationExpr;
class MultiLevelTemplateArgumentList;
class Sema;

/// \brief The result of a constraint satisfaction check, containing the
/// necessary information to diagnose an unsatisfied constraint.
struct ConstraintSatisfaction {
  using SubstitutionDiagnostic = std::pair<SourceLocation, std::string>;
  using Detail = llvm::PointerUnion<Expr *, SubstitutionDiagnostic *>;

  bool IsSatisfied = false;

  /// \brief Pairs of unsatisfied atomic constraint expressions along with the
  /// substituted constraint expr, if the template arguments could be
  /// substituted into them, or a diagnostic if substitution resulted in an
  /// invalid expression.
  llvm::SmallVector<std::pair<const Expr *, Detail>, 4> Details;
};

/// \brief A static requirement that can be used in a requires-expression to
/// check properties of types and expression.
class Requirement {
public:
  // Note - simple and compound requirements are both represented by the same
  // class (ExprRequirement).
  enum RequirementKind { RK_Type, RK_Simple, RK_Compound, RK_Nested };
private:
  const RequirementKind Kind;
  bool Dependent : 1;
  bool ContainsUnexpandedParameterPack : 1;
  bool Satisfied : 1;
public:
  struct SubstitutionDiagnostic {
    std::string SubstitutedEntity;
    SourceLocation DiagLoc;
    std::string DiagMessage;
  };

  Requirement(RequirementKind Kind, bool IsDependent,
              bool ContainsUnexpandedParameterPack, bool IsSatisfied = true) :
      Kind(Kind), Dependent(IsDependent),
      ContainsUnexpandedParameterPack(ContainsUnexpandedParameterPack),
      Satisfied(IsSatisfied) {}

  RequirementKind getKind() const { return Kind; }

  bool isSatisfied() const {
    assert(!Dependent &&
           "isSatisfied can only be called on non-dependent requirements.");
    return Satisfied;
  }

  void setSatisfied(bool IsSatisfied) {
    assert(!Dependent &&
           "setSatisfied can only be called on non-dependent requirements.");
    Satisfied = IsSatisfied;
  }

  void setDependent(bool IsDependent) { Dependent = IsDependent; }
  bool isDependent() const { return Dependent; }

  void setContainsUnexpandedParameterPack(bool Contains) {
    ContainsUnexpandedParameterPack = Contains;
  }
  bool containsUnexpandedParameterPack() const {
    return ContainsUnexpandedParameterPack;
  }

  virtual void Diagnose(Sema &S, bool First) const = 0;

  virtual ~Requirement() = default;
};

/// \brief A requires-expression requirement which queries the existence of a
/// type name or type template specialization ('type' requirements).
class TypeRequirement : public Requirement {
public:
  enum SatisfactionStatus {
      SS_Dependent,
      SS_SubstitutionFailure,
      SS_Satisfied
  };
private:
  llvm::PointerUnion<SubstitutionDiagnostic *, TypeSourceInfo *> Value;
  SatisfactionStatus Status;
public:
  friend class ASTStmtReader;
  friend class ASTStmtWriter;

  /// \brief Construct a type requirement from a type. If the given type is not
  /// dependent, this indicates that the type exists and the requirement will be
  /// satisfied. Otherwise, the SubstitutionDiagnostic constructor is to be
  /// used.
  TypeRequirement(TypeSourceInfo *T);

  /// \brief Construct a type requirement when the nested name specifier is
  /// invalid due to a bad substitution. The requirement is unsatisfied.
  TypeRequirement(SubstitutionDiagnostic *Diagnostic) :
      Requirement(RK_Type, false, false, false), Value(Diagnostic),
      Status(SS_SubstitutionFailure) {}

  SatisfactionStatus getSatisfactionStatus() const { return Status; }
  void setSatisfactionStatus(SatisfactionStatus Status) {
    this->Status = Status;
  }

  bool isSubstitutionFailure() const {
    return Status == SS_SubstitutionFailure;
  }
  SubstitutionDiagnostic *getSubstitutionDiagnostic() const {
    assert(Status == SS_SubstitutionFailure &&
           "Attempted to get substitution diagnostic when there has been no "
           "substitution failure.");
    return Value.get<SubstitutionDiagnostic *>();
  }

  TypeSourceInfo *getType() const {
    assert(!isSubstitutionFailure() &&
           "Attempted to get type when there has been a substitution failure.");
    return Value.get<TypeSourceInfo *>();
  }

  void Diagnose(Sema &S, bool First) const override;

  static bool classof(const Requirement *R) {
    return R->getKind() == RK_Type;
  }
};

/// \brief A requires-expression requirement which queries the validity and
/// properties of an expression ('simple' and 'compound' requirements).
class ExprRequirement : public Requirement {
public:
  enum SatisfactionStatus {
      SS_Dependent,
      SS_ExprSubstitutionFailure,
      SS_NoexceptNotMet,
      SS_TypeRequirementSubstitutionFailure,
      SS_ImplicitConversionAmbiguous,
      SS_NoImplicitConversionExists,
      SS_DeductionFailed,
      SS_ConstraintsNotSatisfied,
      SS_Satisfied
  };
  class ReturnTypeRequirement {
      using ConstrainedParam = std::tuple<TypeSourceInfo *,
                                          TemplateParameterList *,
                                          ConceptSpecializationExpr *>;
      bool Dependent : 1;
      bool ContainsUnexpandedParameterPack : 1;
      llvm::PointerUnion3<ConstrainedParam *, TypeSourceInfo *,
                          SubstitutionDiagnostic *> Value;
  public:
      friend class ASTStmtReader;
      friend class ASTStmtWriter;

      /// \brief No return type requirement was specified.
      ReturnTypeRequirement() : Dependent(false),
                                ContainsUnexpandedParameterPack(false) {}

      /// \brief A return type requirement was specified but it was a
      /// substitution failure.
      ReturnTypeRequirement(SubstitutionDiagnostic *SubstDiag) :
          Dependent(false), ContainsUnexpandedParameterPack(false),
          Value(SubstDiag) {}

      /// \brief A 'trailing return type' style return type requirement.
      ReturnTypeRequirement(ASTContext &C, TypeSourceInfo *ExpectedType);

      /// \brief A 'constrained parameter' style return type requirement.
      /// \param TPL an invented template parameter list containing a single
      /// type parameter and a requires clause.
      /// \param ExpectedType a qualified type referring to the type parameter
      /// of TPL.
      ReturnTypeRequirement(ASTContext &C, TemplateParameterList *TPL,
                            TypeSourceInfo *ExpectedType);

      /// \brief A 'constrained parameter' style return type with
      /// pre-instantiated constraint expression. Should only be used by
      /// ASTStmtReader.
      ReturnTypeRequirement(ASTContext &C, TemplateParameterList *TPL,
                            TypeSourceInfo *ExpectedType,
                            ConceptSpecializationExpr *CSE);

      bool isDependent() const { return Dependent; }

      bool containsUnexpandedParameterPack() const {
        return ContainsUnexpandedParameterPack;
      }

      bool isEmpty() const { return Value.isNull(); }

      bool isSubstitutionFailure() const {
        return Value && Value.is<SubstitutionDiagnostic *>();
      }

      bool isConstrainedParameter() const {
        return Value && Value.is<ConstrainedParam *>();
      }

      bool isTrailingReturnType() const {
        return Value && Value.is<TypeSourceInfo *>();
      }

      SubstitutionDiagnostic *getSubstitutionDiagnostic() const {
        assert(isSubstitutionFailure());
        return Value.get<SubstitutionDiagnostic *>();
      }

      TypeSourceInfo *getTrailingReturnTypeExpectedType() const {
        assert(isTrailingReturnType());
        return Value.get<TypeSourceInfo *>();
      }

      TypeSourceInfo *getConstrainedParamExpectedType() const {
        assert(isConstrainedParameter());
        return std::get<0>(*Value.get<ConstrainedParam *>());
      }

      TemplateParameterList *getConstrainedParamTemplateParameterList() const {
        assert(isConstrainedParameter());
        return std::get<1>(*Value.get<ConstrainedParam *>());
      }

      ConceptSpecializationExpr *getConstrainedParamConstraintExpr() const {
        assert(isConstrainedParameter());
        return std::get<2>(*Value.get<ConstrainedParam *>());
      }

      SatisfactionStatus calculateSatisfaction(Sema &S, Expr *E);
  };
private:
  llvm::PointerUnion<Expr *, SubstitutionDiagnostic *> Value;
  SourceLocation NoexceptLoc; // May be empty if noexcept wasn't specified.
  ReturnTypeRequirement TypeReq;
  SatisfactionStatus Status;
public:
  friend class ASTStmtReader;
  friend class ASTStmtWriter;

  /// \brief Construct a compound requirement, employing semantic analysis to
  /// determine its satisfaction status.
  /// \param E the expression which is checked by this requirement.
  /// \param IsSimple whether this was a simple requirement in source.
  /// \param NoexceptLoc the location of the noexcept keyword, if it was
  /// specified, otherwise an empty location.
  /// \param Req the requirement for the type of the checked expression (omit
  /// if no requirement was specified).
  ExprRequirement(Sema &S, Expr *E, bool IsSimple, SourceLocation NoexceptLoc,
                  ReturnTypeRequirement Req = {});

  /// \brief Construct a compound requirement with a predetermined satisfaction
  /// status.
  /// \param E the expression which is checked by this requirement.
  /// \param IsSimple whether this was a simple requirement in source.
  /// \param NoexceptLoc the location of the noexcept keyword, if it was
  /// specified, otherwise an empty location.
  /// \param Req the requirement for the type of the checked expression.
  ExprRequirement(Expr *E, bool IsSimple, SourceLocation NoexceptLoc,
                  ReturnTypeRequirement Req, SatisfactionStatus Status);

  /// \brief Construct a compound requirement whose expression was a
  /// substitution failure. The requirement is not satisfied.
  /// \param E the diagnostic emitted while instantiating the original
  /// expression.
  /// \param IsSimple whether this was a simple requirement in source.
  /// \param NoexceptLoc the location of the noexcept keyword, if it was
  /// specified, otherwise an empty location.
  /// \param Req the requirement for the type of the checked expression (omit
  /// if no requirement was specified).
  ExprRequirement(SubstitutionDiagnostic *E, bool IsSimple,
                  SourceLocation NoexceptLoc, ReturnTypeRequirement Req = {});

  bool isSimple() const { return getKind() == RK_Simple; }
  bool isCompound() const { return getKind() == RK_Compound; }

  bool hasNoexceptRequirement() const { return NoexceptLoc.isValid(); }
  SourceLocation getNoexceptLoc() const { return NoexceptLoc; }

  SatisfactionStatus getSatisfactionStatus() const { return Status; }

  bool isExprSubstitutionFailure() const {
    return Status == SS_ExprSubstitutionFailure;
  }

  const ReturnTypeRequirement &getReturnTypeRequirement() const {
    return TypeReq;
  }

  SubstitutionDiagnostic *getExprSubstitutionDiagnostic() const {
    assert(isExprSubstitutionFailure() &&
           "Attempted to get expression substitution diagnostic when there has "
           "been no expression substitution failure");
    return Value.get<SubstitutionDiagnostic *>();
  }

  Expr *getExpr() const {
    assert(!isExprSubstitutionFailure() &&
           "ExprRequirement has no expression because there has been a "
           "substitution failure.");
    return Value.get<Expr *>();
  }

  void Diagnose(Sema &S, bool First) const override;

  static bool classof(const Requirement *R) {
    return R->getKind() == RK_Compound || R->getKind() == RK_Simple;
  }
};

/// \brief A requires-expression requirement which is satisfied when a general
/// constraint expression is satisfied ('nested' requirements).
class NestedRequirement : public Requirement {
  Expr *ConstraintExpr;
  ConstraintSatisfaction Satisfaction;

public:
  friend class ASTStmtReader;
  friend class ASTStmtWriter;

  NestedRequirement(Expr *Constraint) :
      Requirement(RK_Nested, /*IsDependent=*/true,
                  Constraint->containsUnexpandedParameterPack()),
      ConstraintExpr(Constraint) {
    assert(Constraint->isInstantiationDependent() && "Non-dependent constraint "
           "expressions must be provided along a Sema& or a precalculated "
           "ConstraintSatisfaction");
  }
  NestedRequirement(Sema &S, Expr *Constraint,
                    const MultiLevelTemplateArgumentList &TemplateArgs);
  NestedRequirement(Expr *Constraint, ConstraintSatisfaction Satisfaction) :
      Requirement(RK_Nested, false, false, Satisfaction.IsSatisfied),
      ConstraintExpr(Constraint), Satisfaction(std::move(Satisfaction)) {}

  Expr *getConstraintExpr() const {
    return ConstraintExpr;
  }

  void Diagnose(Sema &S, bool First) const override;

  static bool classof(const Requirement *R) {
    return R->getKind() == RK_Nested;
  }
};

} // clang

#endif
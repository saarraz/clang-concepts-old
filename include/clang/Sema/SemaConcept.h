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

} // clang

#endif
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

#include "clang/Sema/SemaInternal.h"
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
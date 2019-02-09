// RUN: %clang_cc1 -std=c++2a -fconcepts-ts -x c++ -verify %s
// expected-no-diagnostics

template <typename T>
concept Comparable = requires(T a) { {a == a} -> bool; };

template <typename T>
struct W {
  T val;

  bool operator==(W const& y) const
    requires Comparable<T>
  { return this->val == y.val; }
};

struct X {};
static_assert(!Comparable<X>);
static_assert(!Comparable<W<X>>);
template struct W<X>;
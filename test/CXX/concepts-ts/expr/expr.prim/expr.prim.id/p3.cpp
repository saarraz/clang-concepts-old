// RUN:  %clang_cc1 -std=c++2a -fconcepts-ts -verify %s

template<typename T> concept C1 = true;
static_assert(C1<int>);

template<typename T> concept C2 = sizeof(T) == 4;
static_assert(C2<int>);
static_assert(!C2<long long int>);
static_assert(C2<char[4]>);
static_assert(!C2<char[5]>);

template<typename T> concept C3 = sizeof(*T{}) == 4;
static_assert(C3<int*>);
static_assert(!C3<long long int>);

struct A {
    static constexpr int add(int a, int b) {
        return a + b;
    }
};
struct B {
    static int add(int a, int b) {
        return a + b;
    }
};
template<typename U>
concept C4 = U::add(1, 2) == 3; // expected-error {{substitution into constraint expression resulted in a non-constant expression 'B::add(1, 2) == 3'}}
static_assert(C4<A>);
static bool X = C4<B>; // expected-note {{in concept specialization 'C4<B>'}}

template<typename T, typename U>
constexpr bool is_same_v = false;

template<typename T>
constexpr bool is_same_v<T, T> = true;

template<typename T, typename U>
concept Same = is_same_v<T, U>;

static_assert(Same<int, int>);
static_assert(Same<int, decltype(1)>);
static_assert(!Same<int, unsigned int>);
static_assert(!Same<A, B>);
static_assert(Same<A, A>);

static_assert(Same<bool, decltype(C1<int>)>);
static_assert(Same<bool, decltype(C2<int>)>);
static_assert(Same<bool, decltype(C3<int*>)>);
static_assert(Same<bool, decltype(C4<A>)>);

template<typename T> concept C5 = T{}; // expected-error {{atomic constraint 'int{}' must be of type 'bool' (found 'int')}}
constexpr bool x = C5<int>; // expected-note {{in concept specialization 'C5<int>'}}

template<int x>
concept IsEven = (x % 2) == 0;

static_assert(IsEven<20>);
static_assert(!IsEven<11>);

template<template<typename T> typename P>
concept IsTypePredicate = is_same_v<decltype(P<bool>::value), const bool>
                          && is_same_v<decltype(P<int>::value), const bool>
                          && is_same_v<decltype(P<long long>::value), const bool>;

template<typename T> struct T1 {};
template<typename T> struct T2 { static constexpr bool value = sizeof(T) == 2; };

static_assert(IsTypePredicate<T2>);
static_assert(!IsTypePredicate<T1>);

template<typename T, typename U, typename... Ts>
concept OneOf = (Same<T, Ts> || ...);

template<typename... X>
constexpr bool S = OneOf<X..., int, int>;

static_assert(S<int, long, int>);
static_assert(!S<long, int, char, char>);

namespace piecewise_substitution {
  template <typename T>
  concept True = true;

  template <typename T>
  concept A = True<T> || T::value;

  template <typename T>
  concept B = (True<T> || T::value);

  template <typename T>
  concept C = !True<T> && T::value || true; // expected-warning {{'&&' within '||'}} expected-note {{place parentheses around the '&&' expression to silence this warning}}

  template <typename T>
  concept D = (!True<T> && T::value) || true;

  template <typename T>
  concept E = T::value || True<T>;

  template <typename T>
  concept F = (T::value || True<T>);

  template <typename T>
  concept G = T::value && !True<T> || true; // expected-warning {{'&&' within '||'}} expected-note {{place parentheses around the '&&' expression to silence this warning}}

  template <typename T>
  concept H = (T::value && !True<T>) || true;

  template <typename T>
  concept I = T::value;

  static_assert(A<int>);
  static_assert(B<int>);
  static_assert(C<int>);
  static_assert(D<int>);
  static_assert(E<int>);
  static_assert(F<int>);
  static_assert(G<int>);
  static_assert(H<int>);
  static_assert(!I<int>);
}
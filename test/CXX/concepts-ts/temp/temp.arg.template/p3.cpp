// RUN:  %clang_cc1 -std=c++2a -fconcepts-ts -verify %s

template<typename T> concept C = requires (T t) { t.f(); };
template<typename T> concept D = C<T> && requires (T t) { t.g(); };

template<template<C> class P> struct S1 { }; // expected-note{{'P' declared here}}

template<C> struct X { }; // expected-note{{'X' declared here}}
template<D> struct Y { }; // expected-note 2{{'Y' declared here}}
template<typename T> struct Z { };

S1<X> s11;
S1<Y> s12; // expected-error{{template template argument 'Y' must not be more constrained than template template parameter 'P'}}
S1<Z> s13;

template<template<typename> class P> struct S2 { }; // expected-note 2{{'P' declared here}}

S2<X> s21; // expected-error{{template template argument 'X' must not be more constrained than template template parameter 'P'}}
S2<Y> s22; // expected-error{{template template argument 'Y' must not be more constrained than template template parameter 'P'}}
S2<Z> s23;

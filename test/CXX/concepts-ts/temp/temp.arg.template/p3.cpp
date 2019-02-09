// RUN:  %clang_cc1 -std=c++2a -fconcepts-ts -verify %s

template<typename T> concept C = requires (T t) { t.f(); };
// expected-note@-1{{and here}}
template<typename T> concept D = C<T> && requires (T t) { t.g(); };
template<typename T> concept F = requires (T t) { t.f(); };
// expected-note@-1{{'requires (T t) { t.f(); }' in the two declarations is not considered equivalent - move it to a concept and reference it from here:}}
template<typename T, typename U> concept G = requires (T t) { t.f(); };

template<template<C> class P> struct S1 { }; // expected-note 2{{'P' declared here}}

template<C> struct X { }; // expected-note{{'X' declared here}}
template<D> struct Y { }; // expected-note 2{{'Y' declared here}}
template<typename T> struct Z { };
template<F> struct W { }; // expected-note{{'W' declared here}}
template<G<int>> struct U { }; // expected-note{{'U' declared here}}

S1<X> s11;
S1<Y> s12; // expected-error{{template template argument 'Y' must not be more constrained than template template parameter 'P'}}
S1<Z> s13;
S1<W> s14; // expected-error{{template template argument 'W' must not be more constrained than template template parameter 'P'}}

template<template<typename> class P> struct S2 { }; // expected-note 2{{'P' declared here}}

S2<X> s21; // expected-error{{template template argument 'X' must not be more constrained than template template parameter 'P'}}
S2<Y> s22; // expected-error{{template template argument 'Y' must not be more constrained than template template parameter 'P'}}
S2<Z> s23;

template<typename T, template<G<T>> class P> struct S3 { };
// expected-note@-1{{'P' declared here}}

S3<int, U> s31;
S3<void, U> s32; // expected-error{{template template argument 'U' must not be more constrained than template template parameter 'P'}}

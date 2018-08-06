// RUN: %clang_cc1 -std=c++2a -fconcepts-ts -x c++ %s -verify

template<typename T, int a>
concept C1 = true;

template<C1 T> // expected-error {{concept 'C1' requires more than 1 template argument; provide the remaining arguments explicitly to use it here}} expected-error{{explicit specialization of alias templates is not permitted}}
using badA = T[10];

template<C1<0> T>
using A = T[10];

using a = A<int>;

namespace ns {
  template<typename T, typename U, typename... X>
  concept C2 = true;
}

template<ns::C2 T1, ::ns::C2 T2> // expected-error 2{{concept 'C2' requires more than 1 template argument; provide the remaining arguments explicitly to use it here}}
requires sizeof(T1) <= sizeof(T2) // expected-error{{expected unqualified-id}}
struct badB { };

template<ns::C2<int> T1, ::ns::C2<char, T1> T2>
  requires sizeof(T1) <= sizeof(T2)
struct B { };

using b = B<int, int>;

template<ns::C2... T1> // expected-error {{concept 'C2' requires more than 1 template argument; provide the remaining arguments explicitly to use it here}} expected-error{{extraneous 'template<>' in declaration of struct 'badC'}}
struct badC { };

template<ns::C2<int>... T1>
struct C { };

using c1 = C<char, char, char>;
using c2 = C<char, char, char, char>;
// RUN: %clang_cc1 -std=c++2a -fconcepts-ts -x c++ -verify %s

template<typename T>
struct X {
    using Y = typename T::invalid;
};

template<typename T>
concept Invalid = requires { X<T>{}; };

template<typename T>
concept False = false; // expected-note 2{{because 'false' evaluated to false}}

template<typename T>
concept True = true;

template<True T>
  requires False<T> // expected-note{{because 'int' does not satisfy 'False'}}
void g1() requires Invalid<T>;
// expected-note@-1{{candidate template ignored: constraints not satisfied [with T = int]}}

using g1i = decltype(g1<int>());
// expected-error@-1{{no matching function for call to 'g1'}}

template<False T> // expected-note{{because 'int' does not satisfy 'False'}}
  requires Invalid<T>
void g2(); // requires Invalid<T>;
// expected-note@-1{{candidate template ignored: constraints not satisfied [with T = int]}}

using g2i = decltype(g2<int>());
// expected-error@-1{{no matching function for call to 'g2'}}
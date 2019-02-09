// RUN: %clang_cc1 -std=c++2a -fconcepts-ts -x c++ -verify %s

namespace nodiag {

struct B {
    template <typename T> requires bool(T())
    static int A;
};

template <typename U> requires bool(U())
int B::A = int(U());

} // end namespace nodiag

namespace diag {

struct B {
    template <typename T> requires bool(T()) // expected-note{{template is declared here}}
    static int A;
};

template <typename U> requires !bool(U())  // expected-error{{associated constraints differ in template redeclaration}}
int B::A = int(U());

} // end namespace diag
// RUN: %clang_cc1 -std=c++2a -fconcepts-ts -x c++ -verify %s

namespace nodiag {

template <typename T> requires bool(T())
int A();
template <typename U> requires bool(U())
int A();

} // end namespace nodiag

namespace diag {

template <typename T> requires true // expected-note{{previous template declaration is here}}
int A();
template <typename T> int A(); // expected-error{{associated constraints differ in template redeclaration}}

template <typename T> int B(); // expected-note{{previous template declaration is here}}
template <typename T> requires true // expected-error{{associated constraints differ in template redeclaration}}
int B();

template <typename T> requires true // expected-note{{previous template declaration is here}}
int C();
template <typename T> requires !0 // expected-error{{associated constraints differ in template redeclaration}}
int C();

} // end namespace diag

namespace nodiag {

struct AA {
  template <typename T> requires someFunc(T())
  int A();
};

template <typename T> requires someFunc(T())
int AA::A() { return sizeof(T); }

} // end namespace nodiag

namespace diag {

template <unsigned N>
struct TA {
  template <template <unsigned> class TT> requires TT<N>::happy // expected-note{{previous template declaration is here}}
  int A();
};

template <unsigned N>
template <template <unsigned> class TT> int TA<N>::A() { return sizeof(TT<N>); } // expected-error{{associated constraints differ in template redeclaration}}

} // end namespace diag

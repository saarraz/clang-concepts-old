// RUN: %clang_cc1 -std=c++2a -fconcepts-ts -x c++ -verify %s

namespace nodiag {

template <typename T> requires bool(T())
int A();
template <typename U> requires bool(U())
int A();

} // end namespace nodiag

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
  template <template <unsigned> class TT> requires TT<N>::happy
  int A();
};

template <unsigned N>
template <template <unsigned> class TT> int TA<N>::A() { return sizeof(TT<N>); } // expected-error{{out-of-line definition of 'A' does not match any declaration in 'TA<N>'}}

} // end namespace diag

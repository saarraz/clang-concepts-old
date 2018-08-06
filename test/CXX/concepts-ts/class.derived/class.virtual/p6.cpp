// RUN:  %clang_cc1 -std=c++2a -fconcepts-ts -verify %s

template<typename T>
class A {
  virtual void f1() requires sizeof(T) == 0; // expected-error{{a virtual function must not have a requires clause}}
  virtual void f2() requires sizeof(T) == 1; // expected-error{{a virtual function must not have a requires clause}}
};

template<typename T>
class B : A<T> {
  virtual void f1() requires sizeof(T) == 0 override {} // expected-error{{a virtual function must not have a requires clause}}
};
// RUN: %clang_cc1 -std=c++14 -fconcepts-ts -x c++ %s -verify

// Test parsing of the optional requires-clause in a template-declaration.

template <typename T> requires true
void foo() { }

template <typename T> requires (!0)
struct A {
  void foo();
  struct AA;
  enum E : int;
  static int x;

  template <typename> requires true
  void Mfoo();

  template <typename> requires true
  struct M;

  template <typename> requires true
  static int Mx;

  template <typename TT> requires true
  using MQ = M<TT>;
};

template <typename T> requires (!0)
void A<T>::foo() { }

template <typename T> requires (!0)
struct A<T>::AA { };

template <typename T> requires (!0)
enum A<T>::E : int { E0 };

template <typename T> requires (!0)
int A<T>::x = 0;

template <typename T> requires (!0)
template <typename> requires true
void A<T>::Mfoo() { }

template <typename T> requires (!0)
template <typename> requires true
struct A<T>::M { };

template <typename T> requires (!0)
template <typename> requires true
int A<T>::Mx = 0;

template <typename T> requires true
int x = 0;

template <typename T> requires true
using Q = A<T>;

struct C {
  template <typename> requires true
  void Mfoo();

  template <typename> requires true
  struct M;

  template <typename> requires true
  static int Mx;

  template <typename T> requires true
  using MQ = M<T>;
};

template <typename> requires true
void C::Mfoo() { }

template <typename> requires true
struct C::M { };

template <typename> requires true
int C::Mx = 0;

// Test behavior with non-primary-expression requires clauses

template<typename T> requires foo<T>()
// expected-error@-1{{function call must be parenthesized to be considered part of the requires clause}}
struct B1 { };

int func() { }

template<typename T> requires func()
// expected-error@-1{{atomic constraint must be of type 'bool' (found '<overloaded function type>')}}
// expected-note@-2{{function call must be parenthesized to be considered part of the requires clause}}
struct B2 { };

template<typename T> requires (foo<T>())
struct B3 { };

template<typename T> requires T{}
// expected-error@-1{{expected primary expression before type name; did you forget parentheses?}}
struct B4 { };

template<typename T> requires sizeof(T) == 0
// expected-error@-1{{expected primary expression before sizeof; did you forget parentheses?}}
struct B5 { };

template<typename T> requires (sizeof(T)) == 0
// expected-error@-1{{atomic constraint must be of type 'bool' (found 'unsigned long')}}
// expected-note@-2{{'==' is not considered part of the requires clause (use parentheses to include it)}}
struct B6 { };

template<typename T> requires 0
// expected-error@-1{{atomic constraint must be of type 'bool' (found 'int')}}
(int) bar() { };

template<typename T> requires foo<T>
(int) bar() { };
// expected-error@-1{{function call must be parenthesized to be considered part of the requires clause}}

template<typename T>
void bar() requires foo<T>();
// expected-error@-1{{function call must be parenthesized to be considered part of the requires clause}}

template<typename T>
void bar() requires (foo<T>());

template<typename T>
void bar() requires func();
// expected-error@-1{{atomic constraint must be of type 'bool' (found '<overloaded function type>')}}
// expected-note@-2{{function call must be parenthesized to be considered part of the requires clause}}

template<typename T>
void bar() requires T{};
// expected-error@-1{{expected primary expression before type name; did you forget parentheses?}}

template<typename T>
void bar() requires sizeof(T) == 0;
// expected-error@-1{{expected primary expression before sizeof; did you forget parentheses?}}

template<typename T>
void bar() requires (sizeof(T)) == 0;
// expected-error@-1{{atomic constraint must be of type 'bool' (found 'unsigned long')}}
// expected-note@-2{{'==' is not considered part of the requires clause (use parentheses to include it)}}

void bar(int x, int y) requires (x, y, true);

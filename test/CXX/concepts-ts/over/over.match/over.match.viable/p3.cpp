// RUN:  %clang_cc1 -std=c++2a -fconcepts-ts -verify %s

struct S2 {};
// expected-note@-1 {{candidate constructor (the implicit copy constructor) not viable: no known conversion from 'S1' to 'const S2' for 1st argument}}
// expected-note@-2 {{candidate constructor (the implicit move constructor) not viable: no known conversion from 'S1' to 'S2' for 1st argument}}
// expected-note@-3 {{candidate constructor (the implicit default constructor) not viable: requires 0 arguments, but 1 was provided}}

struct S1 {
  void foo() const requires true {}
  void foo() const requires false {}
  void bar() const requires false {}
  // expected-note@-1 {{because 'false' evaluated to false}}
  operator bool() const requires true { return true; }
  explicit operator bool() const requires false;
  explicit operator S2() const requires false;
  // expected-note@-1 {{candidate function not viable: constraints not satisfied}}
  // expected-note@-2 {{because 'false' evaluated to false}}
};

void foo() {
  S1().foo();
  S1().bar();
  // expected-error@-1 {{invalid reference to function 'bar': constraints not satisfied}}
  (void) static_cast<bool>(S1());
  (void) static_cast<S2>(S1());
  // expected-error@-1 {{no matching conversion for static_cast from 'S1' to 'S2'}}
}
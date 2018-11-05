// RUN: %clang_cc1 -std=c++2a -fconcepts-ts -x c++ %s -verify
#if 0
static_assert(requires { 0; });
static_assert(requires { "aaaa"; });
static_assert(requires { (0).da; }); // expected-error{{member reference base type 'int' is not a structure or union}}

struct A {};
struct B {
    B operator+(const B &other) const { return other; }
};
struct C {
    C operator+(C &other) const { return other; }
};

template<typename T> requires requires (T a, const T& b) { a + b; } // expected-note{{because 'a + b' would be invalid: invalid operands to binary expression ('A' and 'const A')}} expected-note{{because 'a + b' would be invalid: invalid operands to binary expression ('C' and 'const C')}}
struct r1 {};

using r1i1 = r1<int>;
using r1i2 = r1<A>; // expected-error{{constraints not satisfied for class template 'r1' [with T = A]}}
using r1i3 = r1<B>;
using r1i4 = r1<C>; // expected-error{{constraints not satisfied for class template 'r1' [with T = C]}}

struct D { void foo() {} };

template<typename T> requires requires (T a) { a.foo(); } // expected-note{{because 'a.foo()' would be invalid: no member named 'foo' in 'A'}} expected-note{{because 'a.foo()' would be invalid: member reference base type 'int' is not a structure or union}} expected-note{{because 'a.foo()' would be invalid: member function 'foo' not viable: 'this' argument has type 'const D', but function is not marked const}}
struct r2 {};

using r2i1 = r2<int>; // expected-error{{constraints not satisfied for class template 'r2' [with T = int]}}
using r2i2 = r2<A>; // expected-error{{constraints not satisfied for class template 'r2' [with T = A]}}
using r2i3 = r2<D>;
using r2i4 = r2<const D>; // expected-error{{constraints not satisfied for class template 'r2' [with T = const D]}}

template<typename T> requires requires { sizeof(T); } // expected-note{{because 'sizeof(T)' would be invalid: invalid application of 'sizeof' to an incomplete type 'void'}} expected-note{{because 'sizeof(T)' would be invalid: invalid application of 'sizeof' to an incomplete type 'nonexistent'}}
struct r3 {};

using r3i1 = r3<int>;
using r3i2 = r3<A>;
using r3i3 = r3<A &>;
using r3i4 = r3<void>; // expected-error{{constraints not satisfied for class template 'r3' [with T = void]}}
using r3i4 = r3<class nonexistent>; // expected-error{{constraints not satisfied for class template 'r3' [with T = nonexistent]}}

template<typename T> requires requires (T t) { 0; "a"; (void)'a'; }
struct r4 {};

using r4i1 = r4<int>;
using r4i2 = r4<int[10]>;
using r4i3 = r4<int(int)>;
#endif
template<class T> void f(T) = delete;
template<class T> requires sizeof(T) == 1 void f(T) { }

template<typename T> requires requires(T t) { f(t); }
// expected-note@-1{{because 'f(t)' would be invalid: call to deleted function 'f'}}
struct r5 {};

using r5i1 = r5<int>;
// expected-error@-1 {{constraints not satisfied for class template 'r5' [with T = int]}}
using r5i2 = r5<char>;
#if 0
// C++ [expr.prim.req.simple] Example
namespace std_example {
  template<typename T> concept C =
    requires (T a, T b) { // expected-note{{because substituted constraint expression is ill-formed: argument may not have 'void' type}}
      a + b; // expected-note{{because 'a + b' would be invalid: invalid operands to binary expression ('int *' and 'int *')}}
    };

  static_assert(C<int>);
  template<C T> struct C_check {}; // expected-note{{because 'void' does not satisfy 'C'}} expected-note{{because 'int *' does not satisfy 'C'}}
  using c1c1 = C_check<void>; // expected-error{{constraints not satisfied for class template 'C_check' [with T = void]}}
  using c1c2 = C_check<int *>; // expected-error{{constraints not satisfied for class template 'C_check' [with T = int *]}}
}
#endif
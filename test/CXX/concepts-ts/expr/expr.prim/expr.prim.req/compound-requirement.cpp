// RUN: %clang_cc1 -std=c++2a -fconcepts-ts -x c++ %s -verify

static_assert(requires { { 0 }; });
static_assert(requires { { "aaaa" }; });
static_assert(requires { { (0).da }; }); // expected-error{{member reference base type 'int' is not a structure or union}}

void foo() {}
static_assert(requires { { foo() }; });

// Substitution failure in expression

struct A {};
struct B {
    B operator+(const B &other) const { return other; }
};
struct C {
    C operator+(C &other) const { return other; }
};

template<typename T> requires requires (T a, const T& b) { { a + b }; } // expected-note{{because 'a + b' would be invalid: invalid operands to binary expression ('A' and 'const A')}} expected-note{{because 'a + b' would be invalid: invalid operands to binary expression ('C' and 'const C')}}
struct r1 {};

using r1i1 = r1<int>;
using r1i2 = r1<A>; // expected-error{{constraints not satisfied for class template 'r1' [with T = A]}}
using r1i3 = r1<B>;
using r1i4 = r1<C>; // expected-error{{constraints not satisfied for class template 'r1' [with T = C]}}

struct D { void foo() {} };

template<typename T> requires requires (T a) { { a.foo() }; } // expected-note{{because 'a.foo()' would be invalid: no member named 'foo' in 'A'}} expected-note{{because 'a.foo()' would be invalid: member reference base type 'int' is not a structure or union}} expected-note{{because 'a.foo()' would be invalid: member function 'foo' not viable: 'this' argument has type 'const D', but function is not marked const}}
struct r2 {};

using r2i1 = r2<int>; // expected-error{{constraints not satisfied for class template 'r2' [with T = int]}}
using r2i2 = r2<A>; // expected-error{{constraints not satisfied for class template 'r2' [with T = A]}}
using r2i3 = r2<D>;
using r2i4 = r2<const D>; // expected-error{{constraints not satisfied for class template 'r2' [with T = const D]}}

template<typename T> requires requires { { sizeof(T) }; } // expected-note{{because 'sizeof(T)' would be invalid: invalid application of 'sizeof' to an incomplete type 'void'}} expected-note{{because 'sizeof(T)' would be invalid: invalid application of 'sizeof' to an incomplete type 'nonexistent'}}
struct r3 {};

using r3i1 = r3<int>;
using r3i2 = r3<A>;
using r3i3 = r3<A &>;
using r3i4 = r3<void>; // expected-error{{constraints not satisfied for class template 'r3' [with T = void]}}
using r3i4 = r3<class nonexistent>; // expected-error{{constraints not satisfied for class template 'r3' [with T = nonexistent]}}

// Non-dependent expressions

template<typename T> requires requires (T t) { { 0 }; { "a" }; { (void)'a' }; }
struct r4 {};

using r4i1 = r4<int>;
using r4i2 = r4<int[10]>;
using r4i3 = r4<int(int)>;

// Noexcept requirement
void maythrow() { }
static_assert(!requires { { maythrow() } noexcept; });
static_assert(requires { { 1 } noexcept; });

struct E { void operator++(int) noexcept; };
struct F { void operator++(int); };

template<typename T> requires requires (T t) { { t++ } noexcept; } // expected-note{{because 't ++' may throw an exception}}
struct r5 {};

using r5i1 = r5<int>;
using r5i2 = r5<E>;
using r5i2 = r5<F>; // expected-error{{constraints not satisfied for class template 'r5' [with T = F]}}

template<typename T> requires requires (T t) { { t.foo() } noexcept; } // expected-note{{because 't.foo()' would be invalid: no member named 'foo' in 'E'}}
struct r6 {};

using r6i = r6<E>; // expected-error{{constraints not satisfied for class template 'r6' [with T = E]}}

// Trailing return type requirement.

static_assert(requires { { 0 } -> int; { 0 } -> const int; { 0 } -> long long int; });
static_assert(!requires { { 0 } -> void; });
static_assert(!requires { { 0 } -> char[4]; });

struct G { operator F() { return F(); } G(F) {} };
struct H { explicit operator F() { return F(); } };
struct I {
    operator F() { return F(); }
    explicit operator F() const { return F(); }
};
struct J { operator int() { } operator double() { } };

template<typename T, typename U> requires requires (T t) { { t } -> U; } // expected-note{{because expression type 'int' not implicitly convertible to required type 'void'}} expected-note{{because expression type 'H' not implicitly convertible to required type 'F'}} expected-note{{because expression type 'const I' not implicitly convertible to required type 'F'}} expected-note{{because conversion from expression type 'J' to required type 'long' is ambiguous}}
struct r7 {};

using r7i1 = r7<int, char>;
using r7i2 = r7<int, void>; // expected-error{{constraints not satisfied for class template 'r7' [with T = int, U = void]}}
using r7i3 = r7<G, F>;
using r7i4 = r7<H, F>; // expected-error{{constraints not satisfied for class template 'r7' [with T = H, U = F]}}
using r7i5 = r7<F, G>;
using r7i6 = r7<I, F>;
using r7i7 = r7<I, const F>;
using r7i8 = r7<const I, F>; // expected-error{{constraints not satisfied for class template 'r7' [with T = const I, U = F]}}
using r7i9 = r7<J, long int>; // expected-error{{constraints not satisfied for class template 'r7' [with T = J, U = long]}}

struct K { void foo() { } };
struct L { int foo() { } };

template<typename T> requires requires (T t) { { t.foo() } -> void; } // expected-note{{because expression type 'int' not implicitly convertible to required type 'void'}}
struct r8 {};

using r8i1 = r8<K>;
using r8i2 = r8<L>; // expected-error{{constraints not satisfied for class template 'r8' [with T = L]}}

// Substitution failure in return type requirement

struct M { using type = M; };

template<typename T> requires requires (T t) { { t } -> typename T::type; } // expected-note{{because 'typename T::type' would be invalid: type 'int' cannot be used prior to '::' because it has no members}}
struct r9 {};

using r9i1 = r9<M>;
using r9i2 = r9<int>; // expected-error{{constraints not satisfied for class template 'r9' [with T = int]}}

// Constrained parameter

template<typename T, typename U>
constexpr bool is_same_v = false;

template<typename T>
constexpr bool is_same_v<T, T> = true;

template<typename T, typename U>
concept Same = is_same_v<T, U>;

template<typename T>
concept Large = sizeof(T) >= 4; // expected-note{{because 'sizeof(short) >= 4' (2 >= 4) evaluated to false}}

template<typename T> requires requires (T t) { { t } -> Large; } // expected-note{{because expression type 'short' does not satisfy 'Large':}}
struct r10 {};

using r10i1 = r10<int>;
using r10i2 = r10<short>; // expected-error{{constraints not satisfied for class template 'r10' [with T = short]}}

template<typename T> requires requires (T t) { { t } -> Same<T>; }
struct r11 {};

using r11i1 = r11<int>;
using r11i2 = r11<short*>;

template<typename T, typename U> requires requires (T t) { { t } -> const Same<U>*; } // expected-note{{because expression type 'const int' does not match given pattern 'const <type> *'}}
struct r12 {};

using r12i1 = r12<const int, int>; // expected-error{{constraints not satisfied for class template 'r12' [with T = const int, U = int]}}
using r12i2 = r12<M*, M>;
using r12i3 = r12<const M*, M>;

template<typename T, typename U> requires requires (T t) { { t } -> const Same<U> volatile; }
struct r13 {};

using r13i2 = r13<const int volatile, int>;
using r13i3 = r13<const M volatile, M>;
using r13i4 = r13<M const volatile, M>;

template<typename T, typename U> requires requires (T t) { { t } -> Same<U>**; } // expected-note{{because expression type 'const short *' does not match given pattern '<type> **'}}
struct r14 {};

using r14i1 = r14<const short***, const short*>;
using r14i2 = r14<const short*, const short*>; // expected-error{{constraints not satisfied for class template 'r14' [with T = const short *, U = const short *]}}

// Substitution failure in constrained parameter

template<typename T> requires requires (T t) { { t } -> Same<typename T::type>; } // expected-note{{because 'Same<expr-type, typename T::type>' would be invalid: type 'int' cannot be used prior to '::' because it has no members}}
struct r15 {};

using r15i1 = r15<M>;
using r15i2 = r15<int>; // expected-error{{constraints not satisfied for class template 'r15' [with T = int]}}

// Substitution failure in both expression and return type requirement

template<typename T> requires requires (T t) { { t.foo() } -> Same<typename T::type>; } // expected-note{{because 't.foo()' would be invalid: member reference base type 'int' is not a structure or union}}
struct r16 {};

using r16i = r16<int>; // expected-error{{constraints not satisfied for class template 'r16' [with T = int]}}

template<typename T> requires requires (T t) { { t.foo() } -> typename T::type; } // expected-note{{because 't.foo()' would be invalid: member reference base type 'int' is not a structure or union}}
struct r17 {};

using r17i = r17<int>; // expected-error{{constraints not satisfied for class template 'r17' [with T = int]}}

// Non-type concept in constrained parameter

template<int T>
concept IsEven = (T % 2) == 0;

template<typename T> requires requires (T t) { { t } -> IsEven; } // expected-error{{only type concepts can be used to constrain the type of an expression ('IsEven' is a value concept)}}
struct r18 {};

// C++ [expr.prim.req.compound] Example
namespace std_example {
  template<typename T> concept C1 =
    requires(T x) {
      {x++};
    };

  static_assert(C1<int>);
  static_assert(C1<int*>);
  template<C1 T> struct C1_check {};
  using c1c1 = C1_check<int>;
  using c1c2 = C1_check<int[10]>;

  template<typename T> concept C2 =
    requires(T x) {
      {*x} -> typename T::inner; // expected-note{{because '*x' would be invalid: indirection requires pointer operand ('int' invalid)}} expected-note{{because expression type 'int' not implicitly convertible to required type 'typename T2::inner' (aka 'int *')}}
    };

  struct T1 {
    using inner = int;
    inner operator *() { return 0; }
  };
  struct T2 {
    using inner = int *;
    int operator *() { return 0; }
  };
  static_assert(C2<T1>);
  template<C2 T> struct C2_check {}; // expected-note{{because 'int' does not satisfy 'C2'}} expected-note{{because 'std_example::T2' does not satisfy 'C2'}}
  using c2c1 = C2_check<int>; // expected-error{{constraints not satisfied for class template 'C2_check' [with T = int]}}
  using c2c2 = C2_check<T2>; // expected-error{{constraints not satisfied for class template 'C2_check' [with T = std_example::T2]}}

  template<typename T, typename U> concept C3 = false; // expected-note 2{{because 'false' evaluated to false}}
  template<typename T> concept C4 =
    requires(T x) {
      {*x} -> C3<int> const&; // expected-note{{because '*x' would be invalid: indirection requires pointer operand ('int' invalid)}} expected-note {{because type constraint 'C3<int, int>' was not satisfied:}} expected-note {{because type constraint 'C3<short, int>' was not satisfied:}}
    };

  struct T3 {
    short i;
    const short &operator *() { return i; }
  };

  template<C4 T> struct C4_check {}; // expected-note{{because 'int' does not satisfy 'C4'}} expected-note{{because 'int *' does not satisfy 'C4'}} expected-note{{because 'std_example::T3' does not satisfy 'C4'}}
  using c4c1 = C4_check<int>; // expected-error{{constraints not satisfied for class template 'C4_check' [with T = int]}}
  using c4c2 = C4_check<int *>; // expected-error{{constraints not satisfied for class template 'C4_check' [with T = int *]}}
  using c4c3 = C4_check<T3>; // expected-error{{constraints not satisfied for class template 'C4_check' [with T = std_example::T3]}}

  template<typename T>
  void g(T t) noexcept(sizeof(T) == 1) {}

  template<typename T> concept C5 =
    requires(T x) {
      {g(x)} noexcept; // expected-note{{because 'g(x)' may throw an exception}}
    };

  static_assert(C5<char>);
  template<C5 T> struct C5_check {}; // expected-note{{because 'short' does not satisfy 'C5'}}
  using c5 = C5_check<short>; // expected-error{{constraints not satisfied for class template 'C5_check' [with T = short]}}
}
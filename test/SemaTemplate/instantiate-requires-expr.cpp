// RUN: %clang_cc1 -std=c++2a -fconcepts-ts -x c++ %s -verify -Wno-unused-value

template<typename T, typename U>
constexpr bool is_same_v = false;

template<typename T>
constexpr bool is_same_v<T, T> = true;

// We use a hack in this file to make the compiler print out the requires
// expression after it has been instantiated - we put false_v<requires {...}> as
// the requires clause of a class template, then instantiate the template.
// The requirement will not be satisfied, and the explaining diagnostic will
// print out false_v<requires {...}> in its raw form (the false_v serves to
// prevent the diagnostic from elaborating on why the requires expr wasn't
// satisfied).

template<bool v>
constexpr bool false_v = false;

template<typename... Ts>
using void_t = void;

// Check that requires parameters are instantiated correctly.

template<typename T> requires
false_v<requires (T t) { requires is_same_v<decltype(t), int>; }>
// expected-note@-1 {{because 'false_v<requires (int t) { requires is_same_v<decltype(t), int>; }>' evaluated to false}}
// expected-note@-2 {{because 'false_v<requires (char t) { requires is_same_v<decltype(t), int>; }>' evaluated to false}}
struct r1 {};

using r1i1 = r1<int>; // expected-error {{constraints not satisfied for class template 'r1' [with T = int]}}
using r1i2 = r1<char>; // expected-error {{constraints not satisfied for class template 'r1' [with T = char]}}

// Check that parameter packs work.

template<typename... Ts> requires
false_v<requires (Ts... ts) {requires ((sizeof(ts) == 2) && ...);}>
// expected-note@-1 {{because 'false_v<requires (short ts, unsigned short ts) { requires ((sizeof (ts) == 2) && ...); }>'}}
// expected-note@-2 {{because 'false_v<requires (short ts) { requires ((sizeof (ts) == 2) && ...); }>' evaluated to false}}
struct r2 {};

using r2i1 = r2<short, unsigned short>; // expected-error {{constraints not satisfied for class template 'r2' [with Ts = <short, unsigned short>]}}
using r2i2 = r2<short>; // expected-error {{constraints not satisfied for class template 'r2' [with Ts = <short>]}}

template<typename... Ts> requires
false_v<(requires (Ts ts) {requires sizeof(ts) != 0;} && ...)>
// expected-note@-1 {{because 'false_v<requires (short ts) { requires sizeof (ts) != 0; } && requires (unsigned short ts) { requires sizeof (ts) != 0; }>' evaluated to false}}
// expected-note@-2 {{because 'false_v<requires (short ts) { requires sizeof (ts) != 0; }>' evaluated to false}}
struct r3 {};

using r3i1 = r3<short, unsigned short>; // expected-error {{constraints not satisfied for class template 'r3' [with Ts = <short, unsigned short>]}}
using r3i2 = r3<short>; // expected-error {{constraints not satisfied for class template 'r3' [with Ts = <short>]}}

template<typename T>
struct identity { using type = T; };

namespace type_requirement {
  struct A {};

  // check that nested name specifier is instantiated correctly.
  template<typename T> requires false_v<requires { typename T::type; }> // expected-note{{because 'false_v<requires { typename identity<int>::type; }>' evaluated to false}}
  struct r1 {};

  using r1i = r1<identity<int>>; // expected-error{{constraints not satisfied for class template 'r1' [with T = identity<int>]}}

  // check that template argument list is instantiated correctly.
  template<typename T>
  struct contains_template {
      template<typename U> requires is_same_v<contains_template<T>, U>
      using temp = int;
  };

  template<typename T> requires
  false_v<requires { typename T::template temp<T>; }>
  // expected-note@-1 {{because 'false_v<requires { typename contains_template<int>::temp<contains_template<int> >; }>' evaluated to false}}
  // expected-note@-2 {{because 'false_v<requires { typename contains_template<short>::temp<contains_template<short> >; }>' evaluated to false}}
  struct r2 {};

  using r2i1 = r2<contains_template<int>>; // expected-error{{constraints not satisfied for class template 'r2' [with T = type_requirement::contains_template<int>]}}
  using r2i2 = r2<contains_template<short>>; // expected-error{{constraints not satisfied for class template 'r2' [with T = type_requirement::contains_template<short>]}}

  // substitution error occurs, then requires expr is instantiated again

  template<typename T>
  struct a {
      template<typename U> requires (requires { typename T::a::a; }, false)
      // expected-note@-1{{because 'requires { typename <<error-type>>; } , false' evaluated to false}}
      struct r {};
  };

  using ari = a<int>::r<short>; // expected-error{{constraints not satisfied for class template 'r' [with U = short]}}

  // Parameter pack inside expr
  template<typename... Ts> requires
  false_v<(requires { typename Ts::type; } && ...)>
  // expected-note@-1 {{because 'false_v<requires { typename identity<short>::type; } && requires { typename identity<int>::type; } && requires { typename <<error-type>>; }>' evaluated to false}}
  struct r5 {};

  using r5i = r5<identity<short>, identity<int>, short>; // expected-error{{constraints not satisfied for class template 'r5' [with Ts = <identity<short>, identity<int>, short>]}}
  template<typename... Ts> requires
  false_v<(requires { typename void_t<Ts>; } && ...)> // expected-note{{because 'false_v<requires { typename void_t<int>; } && requires { typename void_t<short>; }>' evaluated to false}}
  struct r6 {};

  using r6i = r6<int, short>; // expected-error{{constraints not satisfied for class template 'r6' [with Ts = <int, short>]}}

  template<typename... Ts> requires
  false_v<(requires { typename Ts::template aaa<Ts>; } && ...)>
  // expected-note@-1 {{because 'false_v<requires { typename <<error-type>>; } && requires { typename <<error-type>>; }>' evaluated to false}}
  struct r7 {};

  using r7i = r7<int, A>; // expected-error{{constraints not satisfied for class template 'r7' [with Ts = <int, type_requirement::A>]}}
}

namespace expr_requirement {
  // check that compound/simple requirements are instantiated correctly.

  template<typename T> requires false_v<requires { sizeof(T); { sizeof(T) }; }>
  // expected-note@-1 {{because 'false_v<requires { sizeof(int); { sizeof(int) }; }>' evaluated to false}}
  // expected-note@-2 {{because 'false_v<requires { <<error-expression>>; { sizeof(T) }; }>' evaluated to false}}
  struct r1 {};

  using r1i1 = r1<int>; // expected-error{{constraints not satisfied for class template 'r1' [with T = int]}}
  using r1i2 = r1<void>; // expected-error{{constraints not satisfied for class template 'r1' [with T = void]}}

  // substitution error occurs in expr, then expr is instantiated again.

  template<typename T>
  struct a {
      template<typename U> requires (requires { sizeof(T::a); }, false) // expected-note{{because 'requires { <<error-expression>>; } , false' evaluated to false}}
      struct r {};
  };

  using ari = a<int>::r<short>; // expected-error{{constraints not satisfied for class template 'r' [with U = short]}}

  // check that trailing-return-type requirement is instantiated correctly.

  template<typename T> requires
  false_v<requires { { sizeof(T) } -> typename T::type; }>
  // expected-note@-1 {{because 'false_v<requires { { sizeof(identity<int>) } -> typename identity<int>::type; }>' evaluated to false}}
  struct r2 {};

  using r2i = r2<identity<int>>; // expected-error{{constraints not satisfied for class template 'r2' [with T = identity<int>]}}

  // check that constrained-parameter is instantiated correctly.

  template<typename T, typename U = int>
  concept C1 = is_same_v<T, U>;

  template<typename T> requires false_v<requires(T t) { { t } -> C1<T>; }>
  // expected-note@-1 {{because 'false_v<requires (int t) { { t } -> C1<int>; }>' evaluated to false}}
  // expected-note@-2 {{because 'false_v<requires (double t) { { t } -> C1<double>; }>' evaluated to false}}
  struct r3 {};

  using r3i1 = r3<int>; // expected-error{{constraints not satisfied for class template 'r3' [with T = int]}}
  using r3i2 = r3<double>; // expected-error{{constraints not satisfied for class template 'r3' [with T = double]}}

  void foo(int, double) {}

  template<typename T> requires
  false_v<requires(T t) { { foo } -> C1(int, T); }>
  // expected-note@-1 {{because 'false_v<requires (double t) { { foo } -> C1 (int, double); }>' evaluated to false}}
  // expected-note@-2 {{because 'false_v<requires (int t) { { foo } -> C1 (int, int); }>' evaluated to false}}
  struct r4 {};

  using r4i1 = r4<double>; // expected-error{{constraints not satisfied for class template 'r4' [with T = double]}}
  using r4i2 = r4<int>; // expected-error{{constraints not satisfied for class template 'r4' [with T = int]}}

  // substitution error occurs in return type requirement, then requires expr is
  // instantiated again.

  template<typename T>
  struct b {
      template<typename U> requires (requires { { 0 } -> typename T::a; }, false) // expected-note{{because 'requires { { 0 } -> <<error-type>>; } , false' evaluated to false}}
      struct r {};
  };

  using bri = b<int>::r<short>; // expected-error{{constraints not satisfied for class template 'r' [with U = short]}}

  // Parameter pack inside expr
  template<typename... Ts> requires
  false_v<(requires { { sizeof(Ts) } noexcept -> int; } && ...)>
  // expected-note@-1 {{because 'false_v<requires { { sizeof(int) } noexcept -> int; } && requires { { sizeof(short) } noexcept -> int; }>' evaluated to false}}
  struct r5 {};

  using r5i = r5<int, short>; // expected-error{{constraints not satisfied for class template 'r5' [with Ts = <int, short>]}}

  template<typename... Ts> requires
  false_v<(requires { sizeof(Ts); } && ...)> // expected-note{{because 'false_v<requires { sizeof(int); } && requires { sizeof(short); }>' evaluated to false}}
  struct r6 {};

  using r6i = r6<int, short>; // expected-error{{constraints not satisfied for class template 'r6' [with Ts = <int, short>]}}

  template<typename... Ts> requires
  false_v<(requires { { 0 } noexcept -> Ts; } && ...)>
  // expected-note@-1{{because 'false_v<requires { { 0 } noexcept -> int; } && requires { { 0 } noexcept -> unsigned int; } && requires { { 0 } noexcept -> long; }>' evaluated to false}}
  struct r7 {};

  using r7i = r7<int, unsigned int, long>; // expected-error{{constraints not satisfied for class template 'r7' [with Ts = <int, unsigned int, long>]}}

  template<typename... Ts> requires
  false_v<(requires { { 0 } noexcept -> const C1(Ts); } && ...)>
  // expected-note@-1 {{because 'false_v<requires { { 0 } noexcept -> const C1 (int); } && requires { { 0 } noexcept -> const C1 (unsigned int); }>' evaluated to false}}
  struct r8 {};

  using r8i = r8<int, unsigned int>; // expected-error{{constraints not satisfied for class template 'r8' [with Ts = <int, unsigned int>]}}

  template<typename T, typename U>
  concept C2 = true;

  template<typename... Ts> requires
  false_v<(requires { { 0 } noexcept -> C2<Ts>[3]; } && ...)>
  // expected-note@-1 {{because 'false_v<requires { { 0 } noexcept -> C2<int> [3]; } && requires { { 0 } noexcept -> C2<unsigned int> [3]; }>' evaluated to false}}
  struct r9 {};

  using r9i = r9<int, unsigned int>; // expected-error{{constraints not satisfied for class template 'r9' [with Ts = <int, unsigned int>]}}

  template<typename... Ts> requires
  false_v<(requires { { sizeof(Ts) } noexcept -> C2<Ts>[sizeof(Ts)]; } && ...)>
  // expected-note@-1 {{because 'false_v<requires { { sizeof(int) } noexcept -> C2<int> [4]; } && requires { { sizeof(long) } noexcept -> C2<long> [8]; }>' evaluated to false}}
  struct r10 {};

  using r10i = r10<int, long>; // expected-error{{constraints not satisfied for class template 'r10' [with Ts = <int, long>]}}

  template<typename... Ts> requires
  false_v<(requires { { sizeof(Ts) } noexcept -> const volatile Ts; } && ...)>
  // expected-note@-1 {{because 'false_v<requires { { sizeof(int) } noexcept -> const volatile int; } && requires { { sizeof(unsigned int) } noexcept -> const volatile unsigned int; }>' evaluated to false}}
  struct r11 {};

  using r11i = r11<int, unsigned int>; // expected-error{{constraints not satisfied for class template 'r11' [with Ts = <int, unsigned int>]}}
}

namespace nested_requirement {
  // check that constraint expression is instantiated correctly
  // (substitution doesn't actually occur at the expression level because of [expr.prim.req.nested]p1
  template<typename T> requires false_v<requires { requires sizeof(T) == 2; }> // expected-note{{because 'false_v<requires { requires sizeof(T) == 2; }>' evaluated to false}}
  struct r1 {};

  using r1i = r1<int>; // expected-error{{constraints not satisfied for class template 'r1' [with T = int]}}

  // substitution occurs in expr, then expr is instantiated again.
  template<typename T>
  struct a {
      template<typename U, decltype(requires { requires sizeof(T) + sizeof(U) != 0; }) W>
      struct r {};
  };

  using ari = a<int>::r<int, true>;

  // substitution error occurs in expr, then expr is instantiated again.
  template<typename T>
  struct b {
      template<typename U> requires
      (requires { requires sizeof(T::a) == 0; }, false) // expected-note{{because 'requires { requires sizeof (T::a) == 0; } , false' evaluated to false}}
      struct r {};
  };

  using bri = b<int>::r<short>; // expected-error{{constraints not satisfied for class template 'r' [with U = short]}}

  // Parameter pack inside expr
  template<typename... Ts> requires
  false_v<(requires { requires sizeof(Ts) == 0; } && ...)>
  // expected-note@-1 {{because 'false_v<requires { requires sizeof(Ts) == 0; } && requires { requires sizeof(Ts) == 0; }>' evaluated to false}}
  struct r2 {};

  using r2i = r2<int, short>; // expected-error{{constraints not satisfied for class template 'r2' [with Ts = <int, short>]}}
}

// Parameter pack inside multiple requirements
template<typename... Ts> requires
false_v<(requires { requires sizeof(Ts) == 0; sizeof(Ts); } && ...)>
// expected-note@-1 {{because 'false_v<requires { requires sizeof(Ts) == 0; sizeof(Ts); } && requires { requires sizeof(Ts) == 0; sizeof(Ts); }>' evaluated to false}}
struct r4 {};

using r4i = r4<int, short>; // expected-error{{constraints not satisfied for class template 'r4' [with Ts = <int, short>]}}

template<typename... Ts> requires
false_v<(requires(Ts t) { requires sizeof(t) == 0; t++; } && ...)>
// expected-note@-1 {{because 'false_v<requires (int t) { requires sizeof (t) == 0; t++; } && requires (short t) { requires sizeof (t) == 0; t++; }>' evaluated to false}}
struct r5 {};

using r5i = r5<int, short>; // expected-error{{constraints not satisfied for class template 'r5' [with Ts = <int, short>]}}

template<typename T> requires
false_v<(requires(T t) { T{t}; })> // T{t} creates an "UnevaluatedList" context.
// expected-note@-1 {{because 'false_v<(requires (int t) { int{t}; })>' evaluated to false}}
struct r6 {};

using r6i = r6<int>;
// expected-error@-1 {{constraints not satisfied for class template 'r6' [with T = int]}}

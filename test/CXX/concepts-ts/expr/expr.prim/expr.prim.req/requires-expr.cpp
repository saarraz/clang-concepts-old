// RUN: %clang_cc1 -std=c++2a -fconcepts-ts -x c++ %s -verify

using A = int;

template<typename T, typename U>
constexpr bool is_same_v = false;

template<typename T>
constexpr bool is_same_v<T, T> = true;

static_assert(requires { requires true; 0; typename A; { 0 } -> int; });
static_assert(is_same_v<bool, decltype(requires { requires false; })>);

// Check that requires expr is an unevaluated context.
struct Y {
  int i;
  static constexpr bool r = requires { i; };
};

template<typename T> requires requires (T t) {
  requires false; // expected-note{{because 'false' evaluated to false}}
  requires false; // expected-note{{and 'false' evaluated to false}}
  requires requires {
    requires false; // expected-note{{and 'false' evaluated to false}}
  };
}
struct r1 { };

using r1i = r1<int>; // expected-error{{constraints not satisfied for class template 'r1' [with T = int]}}

template<typename T> requires requires (T t) {
  requires requires {
    requires false; // expected-note{{because 'false' evaluated to false}}
  };
}
struct r2 { };

using r2i = r2<int>; // expected-error{{constraints not satisfied for class template 'r2' [with T = int]}}

template<typename T> requires requires (T t) {
  requires requires {
    requires true;
  };
  requires true;
  requires requires {
    requires false; // expected-note{{because 'false' evaluated to false}}
  };
}
struct r3 { };

using r3i = r3<int>; // expected-error{{constraints not satisfied for class template 'r3' [with T = int]}}

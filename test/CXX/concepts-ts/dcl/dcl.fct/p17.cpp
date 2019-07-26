// RUN:  %clang_cc1 -std=c++2a -fconcepts-ts -verify %s
template<typename T, typename U> constexpr bool is_same_v = false;
template<typename T> constexpr bool is_same_v<T, T> = true;

template<typename... T>
struct type_list;

namespace unconstrained {
  decltype(auto) f1(auto x) { return x; }
  static_assert(is_same_v<decltype(f1(1)), int>);
  static_assert(is_same_v<decltype(f1('c')), char>);

  decltype(auto) f2(auto &x) { return x; }
  // expected-note@-1{{candidate function [with $0 = int] not viable: expects an l-value for 1st argument}}
  // expected-note@-2{{candidate function [with $0 = char] not viable: expects an l-value for 1st argument}}
  static_assert(is_same_v<decltype(f2(1)), int &>); // expected-error{{no matching function for call to 'f2'}}
  static_assert(is_same_v<decltype(f2('c')), char &>); // expected-error{{no matching function for call to 'f2'}}

  decltype(auto) f3(const auto &x) { return x; }
  static_assert(is_same_v<decltype(f3(1)), const int &>);
  static_assert(is_same_v<decltype(f3('c')), const char &>);

  decltype(auto) f4(auto (*x)(auto y)) { return x; } // expected-error{{'auto' only allowed in top-level function declaration parameters}}

  decltype(auto) f5(void (*x)(decltype(auto) y)) { return x; } // expected-error{{'decltype(auto)' not allowed in function prototype}}

  int return_int(); void return_void(); int foo(int);

  decltype(auto) f6(auto (*x)()) { return x; }
  // expected-note@-1{{candidate template ignored: failed template argument deduction}}
  static_assert(is_same_v<decltype(f6(return_int)), int (*)()>);
  static_assert(is_same_v<decltype(f6(return_void)), void (*)()>);
  using f6c1 = decltype(f6(foo)); // expected-error{{no matching function for call to 'f6'}}

  decltype(auto) f7(auto (*x)() -> int) { return x; }
  // expected-note@-1{{candidate function not viable: no known conversion from 'void ()' to 'auto (*)() -> int' for 1st argument}}
  // expected-note@-2{{candidate function not viable: no known conversion from 'int (int)' to 'auto (*)() -> int' for 1st argument}}
  static_assert(is_same_v<decltype(f7(return_int)), int (*)()>);
  using f7c1 = decltype(f7(return_void)); // expected-error{{no matching function for call to 'f7'}}
  using f7c2 = decltype(f7(foo)); // expected-error{{no matching function for call to 'f7'}}
  static_assert(is_same_v<decltype(&f7), int (*(*)(int (*x)()))()>);

  decltype(auto) f8(auto... x) { return (x + ...); }
  static_assert(is_same_v<decltype(f8(1, 2, 3)), int>);
  static_assert(is_same_v<decltype(f8('c', 'd')), int>);
  static_assert(is_same_v<decltype(f8('c', 1)), int>);

  decltype(auto) f9(auto &... x) { return (x, ...); }
  // expected-note@-1{{candidate function [with $0 = <int (), int>] not viable: expects an l-value for 2nd argument}}
  using f9c1 = decltype(f9(return_int, 1)); // expected-error{{no matching function for call to 'f9'}}

  decltype(auto) f11(decltype(auto) x) { return x; } // expected-error{{'decltype(auto)' not allowed in function prototype}}

  template<typename T>
  auto f12(auto x, T y) -> type_list<T, decltype(x)>;
  static_assert(is_same_v<decltype(f12(1, 'c')), type_list<char, int>>);
  static_assert(is_same_v<decltype(f12<char>(1, 'c')), type_list<char, int>>);

  template<typename T>
  auto f13(T x, auto y) -> type_list<T, decltype(y)>;
  static_assert(is_same_v<decltype(f13(1, 'c')), type_list<int, char>>);
  static_assert(is_same_v<decltype(f13<char>(1, 'c')), type_list<char, char>>);

  template<typename T>
  auto f14(auto y) -> type_list<T, decltype(y)>;
  static_assert(is_same_v<decltype(f14<int>('c')), type_list<int, char>>);
  static_assert(is_same_v<decltype(f14<int, char>('c')), type_list<int, char>>);

  template<typename T, typename U>
  auto f15(auto y, U u) -> type_list<T, U, decltype(y)>;
  static_assert(is_same_v<decltype(f15<int>('c', nullptr)), type_list<int, decltype(nullptr), char>>);
  static_assert(is_same_v<decltype(f15<int, decltype(nullptr)>('c', nullptr)), type_list<int, decltype(nullptr), char>>);

  auto f16(auto x, auto y) -> type_list<decltype(x), decltype(y)>;
  static_assert(is_same_v<decltype(f16('c', 1)), type_list<char, int>>);
  static_assert(is_same_v<decltype(f16<int>('c', 1)), type_list<int, int>>);
  static_assert(is_same_v<decltype(f16<int, char>('c', 1)), type_list<int, char>>);

  template<typename T>
  struct S {
    constexpr auto f1(auto x, T t) -> decltype(x + t);

    template<typename U>
    constexpr auto f2(U u, auto x, T t) -> decltype(x + u + t);
  };

  template<typename T>
  constexpr auto S<T>::f1(auto x, T t) -> decltype(x + t) { return x + t; }

  template<typename T>
  template<typename U>
  constexpr auto S<T>::f2(auto x, U u, T t) -> decltype(x + u + t) { return x + u + t; }
  // expected-error@-1 {{out-of-line definition of 'f2' does not match any declaration in 'S<T>'}}

  template<typename T>
  template<typename U>
  constexpr auto S<T>::f2(U u, auto x, T t) -> decltype(x + u + t) { return x + u + t; }

  template<>
  template<>
  constexpr auto S<int>::f2<double>(double u, char x, int t) -> double { return 42; }

  static_assert(S<char>{}.f1(1, 2) == 3);
  static_assert(S<char>{}.f2(1, 2, '\x00') == 3);
  static_assert(S<char>{}.f2<double>(1, 2, '\x00') == 3.);
  static_assert(S<int>{}.f2<double>(1, '2', '\x00') == 42);
}

namespace constrained {
  template<typename T>
  concept C = true;

  void f(C auto x);
  void f(C auto &x);
  void f(const C auto &x);
  void f(C auto (*x)(C auto y)); // expected-error{{'auto' only allowed in top-level function declaration parameters}}
  void f(C auto (*x)(int y));
  void f(C auto (*x)() -> int); // expected-error{{function with trailing return type must specify return type 'auto', not 'C auto'}}
  void f(C auto... x);
  void f(C auto &... x);
  void f(const C auto &... x);
  void f(C decltype(auto) x);
}

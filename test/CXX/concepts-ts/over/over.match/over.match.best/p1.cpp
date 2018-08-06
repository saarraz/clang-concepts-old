// RUN:  %clang_cc1 -std=c++2a -fconcepts-ts -verify %s


template<typename T, typename U>
constexpr static bool is_same_v = false;

template<typename T>
constexpr static bool is_same_v<T, T> = true;

namespace templates
{
  template<typename T>
  concept AtLeast1 = sizeof(T) >= 1;

  template<typename T>
  int foo(T t) requires sizeof(T) == 4 { // expected-note {{candidate function}}
    return 0;
  }

  template<typename T>
  char foo(T t) requires AtLeast1<T> { // expected-note {{candidate function}}
    return 'a';
  }

  template<typename T>
  double foo(T t) requires AtLeast1<T> && sizeof(T) <= 2 {
    return 'a';
  }

  void bar() {
    static_assert(is_same_v<decltype(foo(10)), int>); // expected-error {{call to 'foo' is ambiguous}}
    static_assert(is_same_v<decltype(foo(short(10))), double>);
  }
}

namespace non_template
{
  template<typename T>
  concept AtLeast2 = sizeof(T) >= 2;

  template<typename T>
  concept AtMost8 = sizeof(T) <= 8;

  int foo() requires AtLeast2<long> && AtMost8<long> {
    return 0;
  }

  double foo() requires AtLeast2<long> {
    return 0.0;
  }

  double baz() requires AtLeast2<long> && AtMost8<long> { // expected-note {{candidate function}}
    return 0.0;
  }

  int baz() requires AtMost8<long> && AtLeast2<long> { // expected-note {{candidate function}}
    return 0.0;
  }

  void bar() {
    static_assert(is_same_v<decltype(foo()), int>);
    static_assert(is_same_v<decltype(baz()), int>); // expected-error {{call to 'baz' is ambiguous}}
  }
}


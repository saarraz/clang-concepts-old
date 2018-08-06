// RUN: %clang_cc1 -std=c++2a -fconcepts-ts -x c++ -verify %s

namespace class_templates
{
  template<typename T, typename U> requires sizeof(T) >= 4 // expected-note {{because 'sizeof(char) >= 4' (1 >= 4) evaluated to false}}
  struct is_same { static constexpr bool value = false; };

  template<typename T> requires sizeof(T*) >= 4 && sizeof(T) >= 4
  struct is_same<T*, T*> { static constexpr bool value = true; };

  static_assert(!is_same<char*, char*>::value);
  static_assert(!is_same<short*, short*>::value);
  static_assert(is_same<int*, int*>::value);
  static_assert(is_same<char, char>::value); // expected-error {{constraints not satisfied for class template 'is_same' [with T = char, U = char]}}
}

namespace variable_templates
{
  template<typename T, typename U> requires sizeof(T) >= 4
  constexpr bool is_same_v = false;

  template<typename T> requires sizeof(T*) >= 4 && sizeof(T) >= 4
  constexpr bool is_same_v<T*, T*> = true;

  static_assert(!is_same_v<char*, char*>);
  static_assert(!is_same_v<short*, short*>);
  static_assert(is_same_v<int*, int*>);
}
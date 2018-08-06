// RUN:  %clang_cc1 -std=c++2a -fconcepts-ts -verify %s

template<typename T> requires sizeof(T) >= 4 && sizeof(T) <= 10 // expected-note{{because 'sizeof(char [20]) <= 10' (20 <= 10) evaluated to false}} expected-note{{because 'sizeof(char) >= 4' (1 >= 4) evaluated to false}}
void foo() requires sizeof(T) <= 8 {} // expected-note{{candidate template ignored: constraints not satisfied [with T = char]}} expected-note{{candidate template ignored: constraints not satisfied [with T = char [9]]}} expected-note{{candidate template ignored: constraints not satisfied [with T = char [20]]}} expected-note{{and 'sizeof(char [20]) <= 8' (20 <= 8) evaluated to false}} expected-note{{because 'sizeof(char [9]) <= 8' (9 <= 8) evaluated to false}}

void bar() {
  foo<char>(); // expected-error{{no matching function for call to 'foo'}}
  foo<int>();
  foo<unsigned long long int>();
  foo<char[9]>(); // expected-error{{no matching function for call to 'foo'}}
  foo<char[20]>(); // expected-error{{no matching function for call to 'foo'}}
}
// RUN: %clang_cc1 -std=c++2a -fconcepts-ts -x c++ %s -verify

template<typename T, typename U> constexpr bool is_same_v = false;
template<typename T> constexpr bool is_same_v<T, T> = true;

template<typename T> struct identity { using type = T; };
template<typename T> using identity_t = T;

// Type requirements
template<typename T> requires requires { typename identity_t<T>; }
struct r1;
template<typename U> requires requires { typename identity_t<U>; } // expected-note{{previous template declaration is here}}
struct r1;
template<typename T> requires requires { typename identity_t<T*>; } // expected-error{{associated constraints differ in template redeclaration}}
struct r1;
template<typename T> requires requires { typename ::identity_t<T>; }
struct r1;

template<typename Y> requires requires { typename identity<Y>::type; }
struct r2;
template<typename U> requires requires { typename identity<U>::type; }
struct r2;
template<typename T> requires requires { typename ::identity<T>::type; } // expected-note 2{{previous template declaration is here}}
struct r2;
template<typename T> requires requires { typename identity<T>::typr; } // expected-error{{associated constraints differ in template redeclaration}}
struct r2;
namespace ns {
  template<typename T> struct identity { using type = T; };
}
template<typename T> requires requires { typename ns::identity<T>::type; } // expected-error{{associated constraints differ in template redeclaration}}
struct r2;

template<typename T> requires requires { typename T::template identity<T>::type; }
struct r3;
template<typename U> requires requires { typename U::template identity<U>::type; } // expected-note{{previous template declaration is here}}
struct r3;
template<typename T> requires requires { typename T::template identitr<T>::type; } // expected-error{{associated constraints differ in template redeclaration}}
struct r3;

template<typename T> requires requires { typename T::template temp<>; }
struct r4;
template<typename U> requires requires { typename U::template temp<>; }
struct r4;
template<typename T> requires requires { typename T::template temp; }
struct r4;

// Expr requirements
template<typename T> requires requires { 0; } // expected-note{{previous template declaration is here}}
struct r5;
template<typename T> requires requires { 1; } // expected-error{{associated constraints differ in template redeclaration}}
struct r5;

template<typename T>
concept C1 = true;

template<typename T> requires requires { sizeof(T); }
struct r6;
template<typename U> requires requires { sizeof(U); } // expected-note{{previous template declaration is here}}
struct r6;
template<typename U> requires requires { sizeof(U) - 1; } // expected-error{{associated constraints differ in template redeclaration}}
struct r6;
template<typename T> requires requires { { sizeof(T) }; } // expected-note 3{{previous template declaration is here}}
struct r6;
template<typename T> requires requires { { sizeof(T) } -> decltype(sizeof(int)); } // expected-error{{associated constraints differ in template redeclaration}}
struct r6;
template<typename T> requires requires { { sizeof(T) } noexcept; } // expected-error{{associated constraints differ in template redeclaration}}
struct r6;
template<typename T> requires requires { { sizeof(T) } -> C1; } // expected-error{{associated constraints differ in template redeclaration}}
struct r6;

template<typename T> requires requires { { sizeof(T) } -> int; }
struct r7;
template<typename U> requires requires { { sizeof(U) } -> int; } // expected-note 3{{previous template declaration is here}}
struct r7;
template<typename T> requires requires { { sizeof(T) } -> const int; } // expected-error{{associated constraints differ in template redeclaration}}
struct r7;
template<typename T> requires requires { { sizeof(T) } noexcept -> int; } // expected-error{{associated constraints differ in template redeclaration}}
struct r7;
template<typename T> requires requires { { sizeof(T) } -> T; } // expected-error{{associated constraints differ in template redeclaration}}
struct r7;

template<typename T> requires requires { { sizeof(T) } -> C1; }
struct r8;
template<typename U> requires requires { { sizeof(U) } -> C1; }
struct r8;
template<typename T> requires requires { { sizeof(T) } -> C1<>; } // expected-note 2{{previous template declaration is here}}
struct r8;
template<typename U> requires requires { { sizeof(U) }; } // expected-error{{associated constraints differ in template redeclaration}}
struct r8;
template<typename T> requires requires { { sizeof(T) } -> const C1; } // expected-error{{associated constraints differ in template redeclaration}}
struct r8;

template<typename T> requires requires { { sizeof(T) } -> const volatile C1; }
struct r9;
template<typename U> requires requires { { sizeof(U) } -> const volatile C1; }
struct r9;
template<typename T> requires requires { { sizeof(T) } -> C1 const volatile; }
struct r9;
template<typename T> requires requires { { sizeof(T) } -> C1 volatile const; }
struct r9;
template<typename T> requires requires { { sizeof(T) } -> const C1 volatile; }
struct r9;
template<typename T> requires requires { { sizeof(T) } -> volatile C1 const; }
struct r9;

template<typename T> requires requires { { sizeof(T) } -> C1[sizeof(T)]; }
struct r10;
template<typename U> requires requires { { sizeof(U) } -> C1[sizeof(U)]; } // expected-note 2{{previous template declaration is here}}
struct r10;
template<typename T> requires requires { { sizeof(T) } -> C1[sizeof(T) - 1]; } // expected-error{{associated constraints differ in template redeclaration}}
struct r10;
template<typename T> requires requires { { sizeof(T) } -> C1; } // expected-error{{associated constraints differ in template redeclaration}}
struct r10;

template<typename T> requires requires { { sizeof(T) } -> C1<T>; }
struct r11;
template<typename U> requires requires { { sizeof(U) } -> C1<U>; } // expected-note{{previous template declaration is here}}
struct r11;
template<typename T> requires requires { { sizeof(T) } -> C1<T*>; } // expected-error{{associated constraints differ in template redeclaration}}
struct r11;

// Nested requirements
template<typename T> requires requires { requires sizeof(T) == 0; }
struct r12;
template<typename U> requires requires { requires sizeof(U) == 0; } // expected-note{{previous template declaration is here}}
struct r12;
template<typename T> requires requires { requires sizeof(T) == 1; } // expected-error{{associated constraints differ in template redeclaration}}
struct r12;

// Parameter list
template<typename T> requires requires { requires true; }
struct r13;
template<typename T> requires requires() { requires true; } // expected-note{{previous template declaration is here}}
struct r13;
template<typename T> requires requires(T i) { requires true; } // expected-error{{associated constraints differ in template redeclaration}}
struct r13;

template<typename T> requires requires(T i, T *j) { requires true; } // expected-note 2{{previous template declaration is here}}
struct r14;
template<typename T> requires requires(T i) { requires true; } // expected-error{{associated constraints differ in template redeclaration}}
struct r14;
template<typename T> requires requires(T i, T *j, T &k) { requires true; } // expected-error{{associated constraints differ in template redeclaration}}
struct r14;

// Parameter names
template<typename T> requires requires(int i) { requires sizeof(i) == 1; }
struct r15;
template<typename T> requires requires(int j) { requires sizeof(j) == 1; } // expected-note 2{{previous template declaration is here}}
struct r15;
template<typename T> requires requires(int k) { requires sizeof(k) == 2; } // expected-error{{associated constraints differ in template redeclaration}}
struct r15;
template<typename T> requires requires(const int k) { requires sizeof(k) == 1; } // expected-error{{associated constraints differ in template redeclaration}}
struct r15;

// Order of requirements
template<typename T> requires requires { requires true; 0; }
struct r16;
template<typename T> requires requires { requires true; 0; } // expected-note{{previous template declaration is here}}
struct r16;
template<typename T> requires requires { 0; requires true; } // expected-error{{associated constraints differ in template redeclaration}}
struct r16;
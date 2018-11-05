// RUN: %clang_cc1 -std=c++2a -fconcepts-ts -x c++ %s -verify

bool r1 = requires () {};
// expected-error@-1 {{a requires expression must contain at least one requirement}}

bool r2 = requires { requires true; };

bool r3 = requires (int a, ...) { requires true; };
// expected-error@-1 {{varargs not allowed in requires expression}}

template<typename... T>
bool r4 = requires (T... ts) { requires true; };

bool r5 = requires (bool c, int d) { c; d; };

bool r6 = requires (bool c, int d) { c; d; } && decltype(d){};
// expected-error@-1 {{use of undeclared identifier 'd'}}

bool r7 = requires (bool c) { c; (requires (int d) { c; d; }); d; } && decltype(c){} && decltype(d){};
// expected-error@-1 2{{use of undeclared identifier 'd'}}
// expected-error@-2 {{use of undeclared identifier 'c'}}

bool r8 = requires (bool, int) { requires true; };

bool r9 = requires (bool a, int a) { requires true; };
// expected-error@-1 {{redefinition of parameter 'a'}}
// expected-note@-2 {{previous declaration is here}}

bool r10 = requires (struct new_struct { int x; } s) { requires true; };
// expected-error@-1 {{'new_struct' cannot be defined in a parameter type}}

bool r11 = requires (int x(1)) { requires true; };
// expected-error@-1 {{expected parameter declarator}}
// expected-error@-2 {{expected ')'}}
// expected-note@-3 {{to match this '('}}

bool r12 = requires (int x = 10) { requires true; };
// expected-error@-1 {{default arguments not allowed for parameters of a requires expression}}

bool r13 = requires (int f(int)) { requires true; };

bool r14 = requires (int (*f)(int)) { requires true; };

bool r15 = requires (10) { requires true; };
// expected-error@-1 {{expected parameter declarator}}

bool r16 = requires (auto x) { requires true; };
// expected-error@-1 {{'auto' not allowed in requires expression parameter}}

bool r17 = requires (auto [x, y]) { requires true; };
// expected-error@-1 {{'auto' not allowed in requires expression parameter}}
// expected-error@-2 {{use of undeclared identifier 'x'}}

using a = int;

bool r18 = requires { typename a; };

bool r19 = requires { typename ::a; };

template<typename T> struct identity { using type = T; };

template<typename T> using identity_t = T;

bool r20 = requires {
    typename identity<int>::type;
    typename identity<int>;
    typename ::identity_t<int>;
};

struct s { bool operator==(const s&); ~s(); };

bool r21 = requires { typename s::operator==; };
// expected-error@-1 {{expected identifier or template-id in type requirement}}

bool r22 = requires { typename s::~s; };
// expected-error@-1 {{expected identifier or template-id in type requirement}}

template<typename T>
bool r23 = requires { typename identity<T>::temp<T>; };
// expected-error@-1 {{use 'template' keyword to treat 'temp' as a dependent template name}}

template<typename T>
bool r24 = requires {
    typename identity<T>::template temp<T>;
    typename identity<T>::template temp; // FIXME: This parses fine, should it?
};

bool r25 = requires { ; };
// expected-error@-1 {{expected expression}}

bool r26 = requires { {}; };
// expected-error@-1 {{expected expression}}

bool r27 = requires { { 0 } noexcept; };

bool r28 = requires { { 0 } noexcept noexcept; };
// expected-error@-1 {{expected '->' before expression type requirement}}

bool r29 = requires { { 0 } noexcept int; };
// expected-error@-1 {{expected '->' before expression type requirement}}

template<typename T>
concept C1 = true;

bool r30 = requires { { 0 } noexcept -> const volatile C1; };

bool r31 = requires { { 0 } noexcept -> const C1 volatile; };

bool r32 = requires { { 0 } noexcept -> C1 const volatile; };

bool r33 = requires { { 0 } noexcept -> const volatile C1 const volatile; }; // expected-warning {{duplicate 'const' declaration specifier}} expected-warning {{duplicate 'volatile' declaration specifier}}

bool r34 = requires { { 0 } noexcept -> const volatile C1[1]; };

bool r35 = requires { { 0 } noexcept -> const C1 volatile[1]; };

bool r36 = requires { { 0 } noexcept -> C1 const volatile[1]; };

bool r37 = requires { { 0 } noexcept -> const volatile C1 const volatile[1]; }; // expected-warning {{duplicate 'const' declaration specifier}} expected-warning {{duplicate 'volatile' declaration specifier}}

bool r38 = requires { { 0 } noexcept -> const C1 *(int); };

template<typename T>
T i1 = 0;

bool r39 = requires { { 0 } noexcept -> const volatile i1; };
// expected-error@-1 {{unknown type name 'i1'}}

bool r40 = requires { { 0 } noexcept -> const int; };

bool r41 = requires { { 0 } noexcept -> const volatile decltype(i1<int>); };

template<typename T, typename U>
concept C2 = true;

bool r42 = requires { { 0 } noexcept -> const volatile C2; };
// expected-error@-1 {{concept 'C2' requires more than 1 template argument; provide the remaining arguments explicitly to use it here}}

bool r43 = requires { { 0 } noexcept -> const volatile C1<>; };

bool r44 = requires { { 0 } noexcept -> const volatile C2<int>; };

bool r45 = requires { requires false, 1; };
// expected-error@-1 {{expected ';' at end of requirement}}

bool r46 = requires { 0 noexcept; };
// expected-error@-1 {{'noexcept' can only be used in a compound requirements (with '{' '}' around the expression)}}

bool r47 = requires { 0 int; };
// expected-error@-1 {{unexpected int after expression. Did you intend to use a compound requirement? (with '{' '}' around the expression)}}

bool r48 = requires { requires true };
// expected-error@-1 {{expected ';' at end of requirement}}

bool r49 = requires (bool b) { requires sizeof(b) == 1; };

bool r50 = requires { { 0 } -> auto&&; };
// expected-error@-1 {{'auto' not allowed in expression type requirement}}

bool r51 = requires { { 0 } -> decltype(auto); };
// expected-error@-1 {{'decltype(auto)' not allowed in expression type requirement}}

bool r52 = requires { { 0 } -> const auto *; };
// expected-error@-1 {{'auto' not allowed in expression type requirement}}

void r53(bool b) requires requires { 1 } {}
// expected-error@-1 {{expected ';' at end of requirement}}
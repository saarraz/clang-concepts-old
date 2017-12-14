
// Support parsing of concepts

// RUN:  %clang_cc1 -std=c++2a -fconcepts-ts -verify %s
template<typename T> concept C1 = true;

// TODO: Following line should fail.
template<typename T> concept C1 = true;

template<concept T> concept D1 = true; // expected-error {{expected template parameter}}

template<typename T> concept C2 = 0.f; // expected-error {{constraint expression must be 'bool'}}

struct S1 {
  template<typename T> concept C1 = true; // expected-error {{concept declarations may only appear in global or namespace scope}}
};

template<typename A>
template<typename B>
concept C4 = true; // expected-error {{extraneous template parameter list in concept definition}}

template<typename T> concept C5 = true; // expected-note {{previous}} expected-note {{previous}}
int C5; // expected-error {{redefinition}}
struct C5 {}; // expected-error {{redefinition}}

// TODO: Last of the following two lines should fail.
struct C6 {};
template<typename T> concept C6 = true;

// TODO: Add test to prevent explicit specialization, partial specialization
// and explicit instantiation of concepts.

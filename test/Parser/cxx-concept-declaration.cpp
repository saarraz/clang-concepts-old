
// Support parsing of concepts

// RUN:  %clang_cc1 -std=c++2a -fconcepts-ts -verify %s
template<typename T> concept C1 = true;

// TODO: Following line should fail.
template<typename T> concept C1 = true;

template<concept T> concept D1 = true; // expected-error {{expected template parameter}}

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

template<bool x> concept C7 = 2; // expected-error {{atomic constraint must be of type 'bool' (found 'int')}}
template<bool x> concept C8 = 2 && x; // expected-error {{atomic constraint must be of type 'bool' (found 'int')}}
template<bool x> concept C9 = x || 2 || x; // expected-error {{atomic constraint must be of type 'bool' (found 'int')}}
template<bool x> concept C10 = 8ull && x || x; // expected-error {{atomic constraint must be of type 'bool' (found 'unsigned long long')}}
template<typename T> concept C11 = sizeof(T); // expected-error {{atomic constraint must be of type 'bool' (found 'unsigned long')}}
template<typename T> concept C12 = T{};
static_assert(!C12<bool>);
template<typename T> concept C13 = (bool&&)true;
static_assert(C13<int>);
template<typename T> concept C14 = (const bool&)true;
static_assert(C14<int>);
template<typename T> concept C15 = (const bool)true;
static_assert(C15<int>);

template<typename T, T v>
struct integral_constant { static constexpr T value = v; };

template <bool c> concept C16 = integral_constant<bool, c>::value && true;
static_assert(C16<true>);
static_assert(!C16<false>);
template <bool c> concept C17 = integral_constant<bool, c>::value;
static_assert(C17<true>);
static_assert(!C17<false>);

template <bool word> concept C18 = integral_constant<bool, wor>::value;
// expected-error@-1{{use of undeclared identifier 'wor'; did you mean 'word'?}}
// expected-note@-2{{'word' declared here}}

namespace N {
  template<typename T> concept C19 = true;
}
using N::C19;

static_assert(C19<int>);
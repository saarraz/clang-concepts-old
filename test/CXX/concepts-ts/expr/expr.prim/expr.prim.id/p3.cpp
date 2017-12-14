// RUN:  %clang_cc1 -std=c++2a -fconcepts-ts -verify %s
// expected-no-diagnostics

template<typename T> concept C = true;
static_assert(C<int>);

// RUN:  %clang_cc1 -std=c++2a -fconcepts-ts -verify %s
// expected-no-diagnostics

template<typename T>
struct dont_instantiate { using T::value; };

template<typename T>
struct S1 {
    template<typename U> requires dont_instantiate<T>::value
    struct S2 { };
};

S1<int> s;
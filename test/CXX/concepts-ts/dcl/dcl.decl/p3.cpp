// RUN:  %clang_cc1 -std=c++2a -fconcepts-ts -verify %s

void f1(int a) requires true; // OK
auto f2(int a) -> bool requires true; // OK
auto f3(int a) requires true -> bool; // expected-error{{trailing return type must come before trailing requires clause}}
int f4(int a) requires; // expected-error{{expected expression}}
int f5(int a) requires {} // expected-error{{expected expression}}
void (*pf)() requires true; // expected-error{{trailing requires clause can only be used when declaring a function}}
void g1(int (*dsdads)() requires false); // expected-error{{trailing requires clause can only be used when declaring a function}}
void g2(int (*(*dsdads)())() requires true); // expected-error{{trailing requires clause can only be used when declaring a function}}
void g3(int (*(*dsdads)(int) requires true)() ); // expected-error{{trailing requires clause can only be used when declaring a function}}


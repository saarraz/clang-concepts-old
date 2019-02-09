// RUN: %clang_cc1 -std=c++2a -fconcepts-ts -x c++ -verify %s

template<typename T> requires sizeof(T) >= 4
// expected-note@-1{{'sizeof(T) >= 4' in the two declarations is not considered equivalent - move it to a concept and reference it from here:}}
class A{}; // expected-note{{template is declared here}}

template<typename T> requires sizeof(T) >= 4 && sizeof(T) <= 10
// expected-note@-1{{and here}}
class A<T>{}; // expected-error{{class template partial specialization is not more specialized than the primary template}}

template<typename T>
concept C1 = sizeof(T) >= 4;

template<typename T> requires C1<T>
class B{};

template<typename T> requires C1<T> && sizeof(T) <= 10
class B<T>{};

template<typename T>
concept C2 = sizeof(T) > 1 && sizeof(T) <= 8;

template<typename T>
class C{};

template<typename T> requires C1<T>
class C<T>{};

template<typename T>
class D{}; // expected-note{{previous definition is here}}

template<typename T>
class D<T>{}; // expected-error{{class template partial specialization does not specialize any template argument; to define the primary template, remove the template argument list}} expected-error{{redefinition of 'D'}}

template<typename T> requires C1<T> // expected-note{{template is declared here}}
class E{};

template<typename T> // expected-error{{associated constraints differ in template redeclaration}}
class E<T>{}; // expected-error{{class template partial specialization does not specialize any template argument; to define the primary template, remove the template argument list}}

template<typename T>
struct F{ enum{ value = 1 }; };

template<typename T> requires C1<T> && C2<T>
struct F<T>{ enum{ value = 2 }; };

template<typename T> requires C1<T> || C2<T>
struct F<T>{ enum{ value = 3 }; };

static_assert(F<unsigned>::value == 2);
static_assert(F<char[10]>::value == 3);
static_assert(F<char>::value == 1);

template<typename T1, typename T2>
concept C3 = true;

template<typename... Ts>
concept C4 = C3<Ts...>;

// C4 is normalized to C3<Ts...>@<C4's definition> because there is a pack
// expansion into a non-pack parameter. therefore the two C4<T...> subsume each
// other and the following is non-ambiguous.

template<typename... T> requires C4<T...>
struct G { };

template<typename... T> requires C4<T...> && C4<int, short>
struct G<T...> { };

// Here the two C3s cannot be normalized further, and do not subsume each other
// because they originate in two different locations in code.

template<typename... T> requires C3<T...>
// expected-note@-1{{'C3<T...>' in the two declarations is not considered equivalent - move it to a concept and reference it from here:}}
struct H { }; // expected-note {{template is declared here}}

template<typename... T> requires C3<T...> && C4<int, short>
// expected-note@-1{{and here}}
struct H<T...> { }; // expected-error {{class template partial specialization is not more specialized than the primary template}}

// Make sure atomic constraints subsume each other only if their parameter
// mappings are identical.

template<typename T, typename U> requires C2<T>
struct I { }; // expected-note {{template is declared here}}

template<typename T, typename U> requires C2<U>
struct I<T, U> { }; // expected-error {{class template partial specialization is not more specialized than the primary template}}

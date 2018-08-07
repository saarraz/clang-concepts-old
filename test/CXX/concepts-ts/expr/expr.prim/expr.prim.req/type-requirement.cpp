// RUN: %clang_cc1 -std=c++2a -fconcepts-ts -x c++ %s -verify

using A = int;

template<typename T> using identity_t = T; // expected-note 2{{template is declared here}}}

template<typename T> struct identity { using type = T; };

struct C {};

struct D { static int type; }; // expected-note{{referenced member 'type' is declared here}}

// Basic unqualified and global-qualified lookups

static_assert(requires { typename A; typename ::A; });
static_assert(requires { typename identity_t<A>; typename ::identity_t<A>; });
static_assert(!requires { typename identity_t<A, A>; }); // expected-error{{too many template arguments for alias template 'identity_t'}}
static_assert(!requires { typename ::identity_t<A, A>; }); // expected-error{{too many template arguments for alias template 'identity_t'}}
static_assert(requires { typename identity<A>; });
static_assert(!requires { typename identity; }); // expected-error{{'identity' does not name a type}}
static_assert(!requires { typename ::identity; }); // expected-error{{'::identity' does not name a type}}
static_assert(!requires { typename identity_t; }); // expected-error{{'identity_t' does not name a type}}
static_assert(!requires { typename ::identity_t; }); // expected-error{{'::identity_t' does not name a type}}

namespace ns {
  using B = int;
  int C = 0;
  static_assert(requires { typename A; typename B; typename ::A; });
  static_assert(!requires { typename ns::A; }); // expected-error{{'ns::A' does not name a type}}
  static_assert(!requires { typename ::B; }); // expected-error{{'::B' does not name a type}}
  static_assert(!requires { typename C; }); // expected-error{{'C' does not name a type}}
}

// member type lookups

static_assert(requires { typename identity<int>::type; typename ::identity<int>::type; });
static_assert(!requires { typename identity<int>::typr; }); // expected-error{{'identity<int>::typr' does not name a type}}
static_assert(!requires { typename ::identity<int>::typr; }); // expected-error{{'::identity<int>::typr' does not name a type}}

template<typename T> requires requires { typename T::type; } // expected-note{{because 'T::type' would be invalid: type 'int' cannot be used prior to '::' because it has no members}} expected-note{{because 'T::type' would be invalid: no type named 'type' in 'C'}} expected-note{{because 'T::type' would be invalid: typename specifier refers to non-type member 'type' in 'D'}} expected-note{{in instantiation of template class 'invalid<D>' requested here}} expected-note{{in instantiation of requirement here}} expected-note{{because 'T::type' would be invalid}}
struct r1 {};

using r1i1 = r1<identity<int>>;
using r1i2 = r1<int>; // expected-error{{constraints not satisfied for class template 'r1' [with T = int]}}
using r1i3 = r1<C>; // expected-error{{constraints not satisfied for class template 'r1' [with T = C]}}
using r1i4 = r1<D>; // expected-error{{constraints not satisfied for class template 'r1' [with T = D]}}

template<typename T> struct invalid { typename T::type x; }; // expected-error{{typename specifier refers to non-type member 'type' in 'D'}}
using r1i5 = r1<invalid<D>>; // expected-error{{constraints not satisfied for class template 'r1' [with T = invalid<D>]}} expected-note{{during template argument deduction for class template 'r1' [with T = invalid<D>]}}

// mismatching template arguments

template<typename... Ts> requires requires { typename identity<Ts...>; } // expected-note{{because 'identity<Ts...>' would be invalid: too many template arguments for class template 'identity'}}
struct r2 {};

using r2i1 = r2<int>;
using r2i2 = r2<void>;
using r2i3 = r2<int, int>; // expected-error{{constraints not satisfied for class template 'r2' [with Ts = <int, int>]}}

namespace ns2 {
  template<typename T, typename U> struct identity {};

  template<typename... Ts> requires requires { typename identity<Ts...>; } // expected-note 2{{because 'identity<Ts...>' would be invalid: too few template arguments for class template 'identity'}}
  struct r4 {};

  using r4i1 = r4<int>; // expected-error{{constraints not satisfied for class template 'r4' [with Ts = <int>]}}
}

using r4i2 = ns2::r4<int>; // expected-error{{constraints not satisfied for class template 'r4' [with Ts = <int>]}}

using E = int;
template<typename T> requires requires { typename E<T>; } // expected-error{{template arguments provided for non-template 'E'}}
struct r5v1 {};
template<typename T> requires requires { typename ::E<T>; } // expected-error{{template arguments provided for non-template 'E'}}
struct r5v2 {};

template<typename T> requires sizeof(T) == 1
struct chars_only {};

template<typename T> requires requires { typename chars_only<T>; } // expected-note{{because 'chars_only<T>' would be invalid: constraints not satisfied for class template 'chars_only' [with T = int]}}
struct r6 {};

using r6i = r6<int>; // expected-error{{constraints not satisfied for class template 'r6' [with T = int]}}

template<typename T> int F = 0;

static_assert(!requires { typename F<int>; }); // expected-error{{'F' refers to a variable template and not a type template}}
static_assert(!requires { typename ::F<int>; }); // expected-error{{'::F' refers to a variable template and not a type template}}

struct G { template<typename T> static T temp; };

template<typename T> requires requires { typename T::template temp<int>; } // expected-note{{because 'T::temp<int>' would be invalid: type 'int' cannot be used prior to '::' because it has no members}} expected-note{{because 'T::temp<int>' would be invalid: 'temp' following the 'template' keyword does not refer to a template}} expected-note{{because 'T::temp<int>' would be invalid: template name refers to non-type template 'G::temp'}}
struct r7 {};

using r7i1 = r7<int>; // expected-error{{constraints not satisfied for class template 'r7' [with T = int]}}
using r7i2 = r7<D>; // expected-error{{constraints not satisfied for class template 'r7' [with T = D]}}
using r7i3 = r7<G>; // expected-error{{constraints not satisfied for class template 'r7' [with T = G]}}

template<typename T> struct H;

template<typename T> requires requires { typename H<T>; }
struct r8 {};

using r8i = r8<int>;

template<typename T> struct I { struct incomplete; }; // expected-note{{member is declared here}}

static_assert(!requires { I<int>::incomplete::inner; }); // expected-error{{implicit instantiation of undefined member 'I<int>::incomplete'}}

template<typename T> requires requires { typename I<T>::incomplete::inner; } // expected-note{{because 'I<T>::incomplete::inner' would be invalid: implicit instantiation of undefined member 'I<int>::incomplete'}}
struct r9 {};

using r9i = r9<int>; // expected-error{{constraints not satisfied for class template 'r9' [with T = int]}}

namespace ns3 {
  struct X { }; // expected-note 2{{candidate found by name lookup is 'ns3::X'}}
}

struct X { using inner = int; }; // expected-note 2{{candidate found by name lookup is 'X'}}

using namespace ns3;
static_assert(requires { typename X; }); // expected-error{{reference to 'X' is ambiguous}}
static_assert(requires { typename X::inner; }); // expected-error{{reference to 'X' is ambiguous}}

// naming a type template specialization in a type requirement does not require
// it to be complete and should not care about partial specializations.

template<typename T>
struct Z;

template<typename T> requires sizeof(T) >= 1
struct Z<T> {}; // expected-note{{partial specialization matches [with T = int]}}

template<typename T> requires sizeof(T) <= 4
struct Z<T> {}; // expected-note{{partial specialization matches [with T = int]}}

Z<int> x; // expected-error{{ambiguous partial specializations of 'Z<int>'}}

static_assert(requires { typename Z<int>; });

// C++ [expr.prim.req.type] Example
namespace std_example {
  template<typename T, typename T::type = 0> struct S; // expected-note{{and 'S<T>' would be invalid: type 'int' cannot be used prior to '::' because it has no members}} expected-note{{because 'S<T>' would be invalid: no type named 'type' in 'std_example::has_inner}} expected-note{{and 'S<T>' would be invalid: type 'void' cannot be used prior to '::' because it has no members}}
  template<typename T> using Ref = T&; // expected-note{{and 'Ref<T>' would be invalid: cannot form a reference to 'void'}}
  template<typename T> concept C =
    requires {
      typename T::inner; // expected-note{{because 'T::inner' would be invalid: type 'int' cannot be used prior to '::' because it has no members}} expected-note{{because 'T::inner' would be invalid: no type named 'inner' in 'std_example::has_type'}} expected-note{{because 'T::inner' would be invalid: type 'void' cannot be used prior to '::' because it has no members}}
      typename S<T>;
      typename Ref<T>;
    };

  struct has_inner { using inner = int;};
  struct has_type { using type = int; };
  struct has_inner_and_type { using inner = int; using type = int; };

  static_assert(C<has_inner_and_type>);
  template<C T> struct C_check {}; // expected-note{{because 'int' does not satisfy 'C'}} expected-note{{because 'std_example::has_type' does not satisfy 'C'}} expected-note{{because 'std_example::has_inner' does not satisfy 'C'}} expected-note{{because 'void' does not satisfy 'C'}}
  using c1 = C_check<int>; // expected-error{{constraints not satisfied for class template 'C_check' [with T = int]}}
  using c2 = C_check<has_type>; // expected-error{{constraints not satisfied for class template 'C_check' [with T = std_example::has_type]}}
  using c3 = C_check<has_inner>; // expected-error{{constraints not satisfied for class template 'C_check' [with T = std_example::has_inner]}}
  using c4 = C_check<void>; // expected-error{{constraints not satisfied for class template 'C_check' [with T = void]}}
}
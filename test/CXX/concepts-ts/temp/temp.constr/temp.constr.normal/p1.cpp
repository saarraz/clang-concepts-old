// RUN: %clang_cc1 -std=c++2a -fconcepts-ts -x c++ -verify %s

namespace maybe_incorrect_args {
  template<typename U, typename T>
  concept C = true;

  // diagnostic expected here - the C<Ts...>s are treated as atomic, and since
  // they originate from different source-level constructs, they do not subsume
  // each other.
  template<typename... Ts> requires C<Ts...>
  // expected-note@-1{{'C<Ts...>' in the two declarations is not considered equivalent - move it to a concept and reference it from here:}}
  struct A {}; // expected-note{{template is declared here}}

  template<typename... Ts> requires C<Ts...> && true
  // expected-note@-1{{and here}}
  struct A<Ts...> {}; // expected-error{{class template partial specialization is not more specialized than the primary template}}
}

namespace ill_formed_subst {

  template<typename T>
  struct B; // expected-note 2{{template is declared here}}

  template<typename T>
  concept C1 = true;

  template<typename T, typename U>
  concept C2 = C1<typename B<U>::foo>;
  // expected-error@-1 2{{implicit instantiation of undefined template 'ill_formed_subst::B<int>'}}
  // expected-note@-2 2{{when substituting into C1<typename B<U>::foo>. Make sure concept arguments are valid for any substitution}}

  template<typename T> requires C2<T, int>
  struct A {}; // expected-note {{template is declared here}}

  template<typename T> requires C2<T, int> && true
  struct A<T> {}; // expected-error {{class template partial specialization is not more specialized than the primary template}}
}

namespace incorrect_args_after_subst {
  template<typename T>
  concept C1 = true; // expected-note 2{{template is declared here}}

  template<typename... Ts>
  concept C2 = C1<Ts...>;
  // expected-error@-1 2{{too many template arguments for template 'C1'}}
  // expected-note@-2 2{{when substituting into C1<Ts...>. Make sure concept arguments are valid for any substitution}}

  template<typename T> requires C2<T, T>
  struct A {}; // expected-note{{template is declared here}}

  template<typename T> requires C2<T, T> && true
  struct A<T> {}; // expected-error{{class template partial specialization is not more specialized than the primary template}}
}

namespace maybe_incorrect_args_after_subst {
  template<typename T, typename U>
  concept C1 = true;

  template<typename... Us>
  concept C2 = C1<Us...>;

  // no diagnostic expected here - C1<Us...> is treated as atomic, and since it
  // originates at the same source level construct, the specialized subsumes the
  // primary.
  template<typename... Ts> requires C2<Ts...>
  struct A {};

  template<typename... Ts> requires C2<Ts...> && true
  struct A<Ts...> {};
}

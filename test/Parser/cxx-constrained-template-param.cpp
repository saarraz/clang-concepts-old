// RUN: %clang_cc1 -std=c++2a -fconcepts-ts -x c++ %s -verify
// expected-no-diagnostics

namespace type
{
  template<typename T>
  concept C1 = true;

  template<C1 T, C1 U = int>
  using A = T[10];

  using a = A<int>;

  namespace ns {
    template<typename T, int a = 0>
    concept C2 = true;
  }

  template<ns::C2 T1, ::type::ns::C2 T2> requires sizeof(T1) <= sizeof(T2)
  struct B { };

  using b = B<int, int>;

  template<ns::C2... T1>
  struct C { };

  using c1 = C<char, char, char>;
  using c2 = C<char, char, char, char>;
}

namespace non_type
{
  template<int v>
  concept C1 = true;

  template<C1 v, C1 u = 0>
  int A = v;

  int& a = A<1>;

  namespace ns {
    template<bool x, typename T = int>
    concept C2 = true;
  }

  template<ns::C2 v1, ::non_type::ns::C2 v2> requires sizeof(v1) <= sizeof(v2)
  struct B { };

  using b = B<true, false>;

  template<ns::C2... T1>
  struct C { };

  using c1 = C<false, true, false>;
  using c2 = C<false, true, false, false>;
}

namespace temp
{
  template<typename>
  struct test1 { };

  template<typename>
  struct test2 { };

  template<template<typename> typename T>
  concept C1 = true;

  template<C1 TT, C1 UU = test1>
  using A = TT<int>;

  using a = A<test1>;

  namespace ns {
    template<template<typename> typename... TT>
    concept C2 = true;
  }

  template<ns::C2 TT1, ::temp::ns::C2 TT2>
    requires sizeof(TT1<int>) <= sizeof(TT2<int>)
  struct B { };

  using b = B<test1, test2>;

  template<ns::C2... T1>
  struct C { };

  using c1 = C<test1>;
  using c2 = C<test1, test2, test2>;
}
// RUN:  %clang_cc1 -std=c++2a -fconcepts-ts -verify %s

template<typename T>
[[using clang: opaque]]
concept Large = sizeof(T) > 100;

struct S { };
static_assert(Large<S>);
// expected-error@-1{{static_assert failed}}
// expected-note@-2{{because 'S' does not satisfy 'Large'}}

#include <type_traits>

namespace detail {
  template<typename T, typename U>
  concept SameHelper = std::is_same_v<T, U>;
}

template<typename T, typename U>
[[using clang: opaque, relation("the", "as")]]
concept Same = detail::SameHelper<T, U> && detail::SameHelper<U, T>;

static_assert(Same<int, long>);
// expected-error@-1{{static_assert failed}}
// expected-note@-2{{because 'int' is not the 'Same' as 'long'}}

template<typename T, typename U>
[[using clang: opaque, relation("the")]]
// expected-error@-1{{'relation' attribute requires exactly 2 arguments}}
concept BadRelation = false;

template<typename T>
[[using clang: opaque, noun]]
concept Object = std::is_object_v<T>;

static_assert(Object<int&>);
// expected-error@-1{{static_assert failed}}
// expected-note@-2{{because 'int &' is not an 'Object'}}

template<typename T>
[[using clang: opaque, noun]]
concept SmallType = sizeof(T) < 4;

static_assert(SmallType<int>);
// expected-error@-1{{static_assert failed}}
// expected-note@-2{{because 'int' is not a 'SmallType'}}

template<typename T>
[[using clang: opaque, noun(an)]]
concept SmallerType = sizeof(T) < 2;

static_assert(SmallerType<int>);
// expected-error@-1{{static_assert failed}}
// expected-note@-2{{because 'int' is not an 'SmallerType'}}

template<typename T>
[[using clang: opaque, noun(ds)]]
// expected-error@-1{{argument to noun, if provided, must be either 'a' or 'an'}}
concept BadConcept1 = false;

template<typename T>
[[using clang: opaque, noun(false)]]
// expected-error@-1{{argument to noun, if provided, must be either 'a' or 'an'}}
concept BadConcept2 = false;

template<typename T>
[[clang::noun(a)]]
concept UsefulObject = Object<T> && sizeof(T) > 10;
// expected-note@-1{{because 'sizeof(int) > 10' (4 > 10) evaluated to false}}

static_assert(UsefulObject<int>);
// expected-error@-1{{static_assert failed}}
// expected-note@-2{{because 'int' is not a 'UsefulObject'}}

template<typename T>
[[clang::adjective]]
concept Fooable = requires (T t) { t.foo(); t.bar(); };
// expected-note@-1{{because substituted constraint expression is ill-formed: argument may not have 'void' type}}

static_assert(Fooable<void>);
// expected-error@-1{{static_assert failed}}
// expected-note@-2{{because 'void' is not 'Fooable'}}
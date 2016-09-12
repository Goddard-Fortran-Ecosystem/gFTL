module FooVector_mod
   use Foo_mod
#define _entry type(Foo)
#define EQUAL_DEFINED
#include <templates/vector.inc>
end module FooVector_mod

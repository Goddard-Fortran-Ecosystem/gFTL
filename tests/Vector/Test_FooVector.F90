#define _entry type(Foo)
#define EQUAL_DEFINED

#define SUITE Test_FooVector
#define SUITE_NAME 'Test_FooVector'

#define _ONE Foo(1)
#define _TWO Foo(2)
#define _THREE Foo(3)
#define _FOUR Foo(4)
#define _FIVE Foo(5)

module Test_FooVector_mod
   use pFUnit_mod
   use Foo_mod
   use FooVector_mod
   implicit none
   private

   public :: SUITE

#include <genericTestVector.inc>

end module Test_FooVector_mod

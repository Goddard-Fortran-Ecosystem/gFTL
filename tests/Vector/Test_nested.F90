module A_mod
#include <types/integer.inc>
#define _vector IntegerVector
#include <templates/vector.inc>
#undef _equal_defined
#undef _less_than_defined

end module A_mod

module B_mod
   use A_mod, only: IntegerVector
   
#define _value class(IntegerVector)
#define _value_allocatable
#include <types/key_deferredLengthString.inc>
#define _alt
#include <templates/map.inc>

   
end module B_mod
   

! 7/6/2018 Under Intel 18, some nested compilers were triggering a compiler bug
! which is reladet to having a type-bound ASSIGNMENT(=) procedure.  A simplified
! reproducer has been submitted to Intel, and 19 beta no longer has the issue.


module Test_Nested_mod
   use pFUnit_mod
   use A_mod
   use B_mod
contains

   subroutine test_nested_vector()
      implicit none

      type (IntegerVector) :: v
      type (Map), target :: m
      
      call v%push_back(1)
      call m%insert('foo', v)
      call assertTrue(associated(m%at('foo')))
   end subroutine test_nested_vector

   function test_nested_suite() result(suite)
      type (TestSuite) :: suite
      
      suite = newTestSuite('Test_nested')
      
      call suite%addTest(newTestMethod('test_nested_vector', test_nested_vector))
   end function test_nested_suite
   
end module Test_Nested_mod

      

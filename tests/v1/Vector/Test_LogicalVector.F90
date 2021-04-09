#include <intrinsics/logical.inc>
#define SUITE Test_LogicalVector
#define SUITE_NAME 'Test_LogicalVector'


module Test_LogicalVector_mod
   use pfunit_mod
   use LogicalVector_mod
   implicit none
   private

   public :: SUITE

#include <genericTestVector.inc>

end module Test_LogicalVector_mod

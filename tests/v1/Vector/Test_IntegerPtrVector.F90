#include <intrinsics/integerPtr.inc>
#define SUITE Test_IntegerPtrVector
#define SUITE_NAME 'Test_IntegerPtrVector'

module Test_IntegerPtrVector_mod
   use pfunit_mod
   use IntegerPtrVector_mod
   implicit none
   private

   public :: SUITE

#include <genericTestVector.inc>

end module Test_IntegerPtrVector_mod

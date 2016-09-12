#include <intrinsics/integer.inc>
#define SUITE Test_IntegerVector
#define SUITE_NAME 'Test_IntegerVector'


module Test_IntegerVector_mod
   use pfunit_mod
   use IntegerVector_mod
   implicit none
   private

   public :: SUITE

#include <genericTestVector.inc>

end module Test_IntegerVector_mod

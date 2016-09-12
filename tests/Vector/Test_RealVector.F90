#include <intrinsics/real.inc>
#define SUITE Test_RealVector
#define SUITE_NAME 'Test_RealVector'

module Test_RealVector_mod
   use pfunit_mod
   use RealVector_mod
   implicit none
   private

   public :: SUITE

#include <genericTestVector.inc>

end module Test_RealVector_mod

module Test_Real64Vector_mod

#include <intrinsics/real64.inc>
#define SUITE Test_Real64Vector
#define SUITE_NAME 'Test_Real64Vector'

   use pfunit_mod
   use Real64Vector_mod
   implicit none
   private

   public :: SUITE

#include <genericTestVector.inc>

end module Test_Real64Vector_mod

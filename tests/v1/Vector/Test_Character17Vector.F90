#include <intrinsics/character17.inc>
#define SUITE Test_Character17Vector
#define SUITE_NAME 'Test_Character17Vector'


module Test_Character17Vector_mod
   use pfunit_mod
   use Character17Vector_mod
   implicit none
   private

#ifndef __INTEL_COMPILER
   public :: SUITE

#include <genericTestVector.inc>
#endif
end module Test_Character17Vector_mod

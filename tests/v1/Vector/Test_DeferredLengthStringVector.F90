#include <intrinsics/deferredLengthString.inc>
#define SUITE Test_DeferredLengthStringVector
#define SUITE_NAME 'Test_DeferredLengthStringVector'


module Test_DeferredLengthStringVector_mod
   use pfunit_mod
   use DeferredLengthStringVector_mod
   implicit none
   private

#ifndef __GFORTRAN__
   public :: SUITE

#include <genericTestVector.inc>
#endif

end module Test_DeferredLengthStringVector_mod

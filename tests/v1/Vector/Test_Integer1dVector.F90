#include <intrinsics/integer1d.inc>
#define SUITE Test_Integer1dVector
#define SUITE_NAME 'Test_Integer1dVector'

module Test_Integer1dVector_mod
   use pfunit_mod
   use Integer1dVector_mod
   implicit none
   private

   public :: SUITE

#include <genericTestVector.inc>

end module Test_Integer1dVector_mod

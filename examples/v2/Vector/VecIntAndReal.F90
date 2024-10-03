! This is an example for integer and real vector. 
! It can be changed to any intrinsic type easliy.
! 1) create a module for each type
! 2) use alias when using different  vector types

module IntegerVec_mod
#define T integer
#define Vector VectorInt
#define VectorIterator VectorIntIterator
#include "vector/template.inc"
#undef T
#undef Vector
#undef VectorIterator
end module IntegerVec_mod

module Real64Vec_mod
use, intrinsic :: iso_fortran_env

#define T real
#define T_KINDLEN(context) (kind=real64)
#include "vector/template.inc"
#undef T
#undef T_KINDLEN
end module Real64Vec_mod

module Real32Vec_mod
use, intrinsic :: iso_fortran_env

#define T real
#define T_KINDLEN(context) (kind=real32)
#include "vector/template.inc"
#undef T
#undef T_KINDLEN
end module Real32Vec_mod

program main

   use IntegerVec_mod
   use Real64Vec_mod,only :VectorReal64=>Vector
   use, intrinsic :: iso_fortran_env

   implicit none
   type (VectorInt) :: iv
   type (VectorReal64) :: rv
   real(real64) :: r

   iv=VectorInt()
   rv=VectorReal64()

   r = 1.0d0
   call iv%push_back(1)
   call rv%push_back(r)
   call check(iv,1)
   call checkr(rv,r)
contains

   subroutine check(iv, expected)
      type(VectorInt), intent(in) :: iv
      integer, intent(in) :: expected

      print*,"iv%at('",1,"')  = ",iv%at(1),"(should be",expected,")"
   end subroutine check

   subroutine checkr(rv, expected)
      type(VectorReal64), intent(in) :: rv
      real(real64), intent(in) :: expected

      print*,"rv%at('",1,"')  = ",rv%at(1),"(should be",expected,")"
   end subroutine checkr
end program main


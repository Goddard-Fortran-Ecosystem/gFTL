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

#define T __REAL64
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
   type (VectorInt) :: iv_range
   type (VectorIntIterator) :: iv_iter
   real(real64) :: r

   iv=VectorInt()
   rv=VectorReal64()
   iv_range=VectorInt([1,2,3,4,5,6,7,8,9,10])

   r = 1.0d0
   call iv%push_back(1)
   call rv%push_back(r)
   call iv_range%push_back(20)
   call check(iv,1,1)
   call checkr(rv,1,r)
   print *, "iv_range%size() = ", iv_range%size()
   call check(iv_range, 1, 1)
   call check(iv_range, 10, 10)
   call check(iv_range, 11, 20)
   iv_iter = iv_range%begin() + 5
   iv_iter = iv_range%insert(iv_iter, 30)
   call check(iv_range, 6, 30)
   call check(iv_range, 7, 6)
   call check(iv_range, 12, 20)
contains

   subroutine check(iv, idx, expected)
      type(VectorInt), intent(in) :: iv
      integer, intent(in) :: idx
      integer, intent(in) :: expected

      print*,"iv%at('",idx,"')  = ",iv%at(idx),"(should be",expected,")"
   end subroutine check

   subroutine checkr(rv, idx, expected)
      type(VectorReal64), intent(in) :: rv
      integer, intent(in) :: idx
      real(real64), intent(in) :: expected

      print*,"rv%at('",idx,"')  = ",rv%at(idx),"(should be",expected,")"
   end subroutine checkr
end program main


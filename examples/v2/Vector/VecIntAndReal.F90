! This is an example for integer and real vector. 
! It can be changed to any intrinsic type easliy.
! 1) create a module for each type
! 2) use alias when using different  vector types

!----------------------------------------
! Module creating a vector of integers
!----------------------------------------
module IntegerVec_mod
! Specify the type of the elements of the vector
#define T integer
! Specify the name of the vector type (Vector by default)
#define Vector VectorInt
! Specify the name of the vector iterator type (VectorIterator by default)
#define VectorIterator VectorIntIterator
! Include the vector template file to define the vector type
#include "vector/template.inc"
! Undefine macros to tidy up
#undef T
#undef Vector
#undef VectorIterator
end module IntegerVec_mod

!----------------------------------------
! Module creating a vector of real64
!----------------------------------------
module Real64Vec_mod
! Include any modules needed for the types used in the vector
use, intrinsic :: iso_fortran_env, only: REAL64
! Specify the type of the elements of the vector
! The shortcut __REAL64 is used to specify reals with a 64 bit precision as defined in iso_fortran_env
#define T __REAL64
! Include the vector template file to define the vector type
! The type will be called Vector
#include "vector/template.inc"
! Undefine macros to tidy up
#undef T
end module Real64Vec_mod

!----------------------------------------
! Module creating a vector of real32
!----------------------------------------
module Real32Vec_mod
! Include any modules needed for the types used in the vector
use, intrinsic :: iso_fortran_env

! Specify the type of the elements of the vector
#define T real
! Specify the precision of the elements of the vector
#define T_KINDLEN(context) (kind=real32)
! Include the vector template file to define the vector type
! The type will be called Vector
#include "vector/template.inc"
! Undefine macros to tidy up
#undef T
#undef T_KINDLEN
end module Real32Vec_mod



program main

   ! Import everything related to the VectorInt type
   use IntegerVec_mod
   ! In order to import the vectors of floats the vector must be renamed to avoid name collisions
   use Real64Vec_mod,only :VectorReal64=>Vector
   use Real32Vec_mod,only :VectorReal32=>Vector

   use, intrinsic :: iso_fortran_env

   implicit none
   type (VectorInt) :: iv
   type (VectorReal64) :: rv
   type (VectorInt) :: iv_range
   type (VectorIntIterator) :: iv_iter
   real(real64) :: r

   ! Initialise the vector types
   iv=VectorInt()
   rv=VectorReal64()
   iv_range=VectorInt([1,2,3,4,5,6,7,8,9,10])

   ! Add elements to the end of the vectors
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

   ! Add an elements to the vector after the 5th element
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


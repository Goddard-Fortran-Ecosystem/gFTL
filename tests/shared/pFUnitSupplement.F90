!---------------------------------------------------------------------------
! AssertEqual cannot be overloaded for unlimited polymorphic, as it would
! lead to ambiguous interface.  A future pFUnit could provide only
! unlimited poly and then do selection under the hood.  
!
! For the special purpose of testing Unlimited Polymorphic containers,
! this module should be used.
!---------------------------------------------------------------------------


module pFUnitSupplement_mod
   use pFUnit_mod, only: shadowAssert => assertEqual
   use pFUnit_mod, only: SourceLocation
   use pFUnit_mod, only: throw
   use iso_fortran_env, only: INT64
   use iso_fortran_env, only: REAL64
   implicit none
   private

   public :: assertEqual

   interface assertEqual
      module procedure assertEqual_unlimited
!!$      module procedure assertEqual_unlimitedArray
   end interface assertEqual

contains


   subroutine assertEqual_unlimited(a, b, location)
      class (*), intent(in) :: a
      class (*), intent(in) :: b
      type (SourceLocation), intent(in) :: location

      select type (pa => a)

      type is (integer)
         select type (pb => b)
         type is (integer)
            call shadowAssert(pa, pb, location=location)
         type is (integer(kind=INT64))
            call shadowAssert(pa, pb, location=location)
         class default
            call throw('inconsistent type', location=location)
         end select

      type is (integer(kind=INT64))
         select type (pb => b)
         type is (integer)
            call shadowAssert(pa, pb, location=location)
         type is (integer(kind=INT64))
            call shadowAssert(pa, pb, location=location)
         class default
            call throw('inconsistent type', location=location)
         end select

      type is (real)
         select type (pb => b)
         type is (real)
            call shadowAssert(pa, pb, location=location)
         class default
            call throw('inconsistent type', location=location)
         end select

      type is (real(REAL64))
         select type (pb => b)
         type is (real(REAL64))
            call shadowAssert(pa, pb, location=location)
         class default
            call throw('inconsistent type', location=location)
         end select

      type is (complex)
         select type (pb => b)
         type is (complex)
            call shadowAssert(pa, pb, location=location)
         class default
            call throw('inconsistent type', location=location)
         end select

      class default 
         call throw('unsupported type', location=location)
      end select

   end subroutine assertEqual_unlimited


!!$   subroutine assertEqual_unlimitedArray(a, b, location)
!!$      type (XWrap), intent(in) :: a(:)
!!$      type (XWrap), intent(in) :: b(:)
!!$      type (SourceLocation), intent(in) :: location
!!$
!!$      integer :: i
!!$
!!$      call assertEqual(shape(a), shape(b), message='different shape', location=location)
!!$      if (anyExceptions()) return
!!$
!!$      do i = 1, size(a)
!!$         select type (pa => a(i)%item)
!!$         type is (integer)
!!$            select type (pb => b(i)%item)
!!$            type is (integer)
!!$               call assertEqual(pa, pb, location=location)
!!$            end select
!!$         end select
!!$         if (anyExceptions()) return
!!$      end do
!!$
!!$
!!$   end subroutine assertEqual_unlimitedArray

end module pFUnitSupplement_mod

!--------------------------------------------------------------------
! Copyright © 2020 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

!---------------
! The sequence of includes below seems a bit of overkill in the
! case of Pair containers.  However, it becomes necessary when
! considering higher order containers that use lower order
! containers.  E.g., Set uses Vector and Map uses Pair.
!---------------

! define generic internal template parameters from pair parameters.
#include "parameters/T/copy_ptr_T_to_internal_T.inc"
#define __guard __ptr_guard

! define derived generic template parameters from internal parameters.
#include "parameters/T/define_derived_macros.inc"

   function __MANGLE(new_ptr)(trgt) result(p)
      type (__ptr) :: p
      __T_declare_component__, target, intent(in) :: trgt
      p%ptr => trgt
   end function __MANGLE(new_ptr)

   function __MANGLE(get)(this) result(p)
      __T_declare_result__, pointer :: p
      class(__ptr), intent(in) :: this

      p => this%ptr
   end function __MANGLE(get)


   ! =======================
   !  equal
   ! =======================
   logical function __MANGLE(equal)(a, b) result(equal)
      type(__ptr), intent(in) :: a
      type(__ptr), intent(in) :: b

      equal = associated(a%ptr, b%ptr)

      return
   end function __MANGLE(equal)


   ! =======================
   !  not_equal
   ! =======================
   logical function __MANGLE(not_equal)(a, b) result(not_equal)
      type(__ptr), intent(in) :: a
      type(__ptr), intent(in) :: b

      not_equal = .not. (a == b)

      return
   end function __MANGLE(not_equal)


   subroutine __MANGLE(swap)(a, b)
      type(__ptr), intent(inout) :: a
      type(__ptr), intent(inout) :: b

      __T_declare_component__, pointer :: p

      p => a%ptr
      a%ptr => b%ptr
      b%ptr => p

   end subroutine __MANGLE(swap)

   ! undef derived template parameters
#include "parameters/T/undef_derived_macros.inc"

   ! undef internal template parameters
#undef __guard
#include "parameters/T/undef_internal.inc"


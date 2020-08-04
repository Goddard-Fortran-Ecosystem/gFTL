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
#include "parameters/T1/copy_pair_T1_to_internal_T1.inc"
#include "parameters/T2/copy_pair_T2_to_internal_T2.inc"
#define __guard __pair_guard

! define derived generic template parameters from internal parameters.
#include "parameters/T1/define_derived_macros.inc"
#include "parameters/T2/define_derived_macros.inc"

function __MANGLE(get_first)(this) result(first)
   __T1_declare_result__, pointer :: first
   class(__pair), target, intent(in) :: this

   first => this%first
end function __MANGLE(get_first)

function __MANGLE(get_second)(this) result(second)
   __T2_declare_result__, pointer :: second
   class(__pair), target, intent(in) :: this

   second => this%second
end function __MANGLE(get_second)

subroutine __MANGLE(swap)(a, b)
   type(__pair), intent(inout) :: a
   type(__pair), intent(inout) :: b

   __T1_declare_component__ :: tmp_first
   __T2_declare_component__ :: tmp_second

   __T1_MOVE__(tmp_first,a%first)
   __T1_MOVE__(a%first,b%first)
   __T1_MOVE__(b%first,tmp_first)
   
   __T2_MOVE__(tmp_second,a%second)
   __T2_MOVE__(a%second,b%second)
   __T2_MOVE__(b%second,tmp_second)
   
end subroutine __MANGLE(swap)
         
! undef derived template parameters
#include "parameters/T1/undef_derived_macros.inc"
#include "parameters/T2/undef_derived_macros.inc"

! undef internal template parameters
#undef __guard
#include "parameters/T1/undef_internal.inc"
#include "parameters/T2/undef_internal.inc"


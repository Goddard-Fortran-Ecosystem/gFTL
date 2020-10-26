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

   function __MANGLE(new_pair)(first,second) result(p)
      type (Pair) :: p
      __T1_declare_dummy__, intent(in) :: first
      __T2_declare_dummy__, intent(in) :: second
      p%first = first
      p%second = second
   end function __MANGLE(new_pair)

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


#if (defined(__T1_LT__) || defined(__T1_EQ__)) && (defined(__T2_LT__) || defined(__T2_EQ__))
   ! =======================
   !  equal
   ! =======================
   logical function __MANGLE(equal)(a, b) result(equal)
      type(__pair), target, intent(in) :: a
      type(__pair), target, intent(in) :: b

#if defined(__T1_EQ__)
      equal = __T1_EQ__(a%first,b%first)
#else
      equal = .not. (__T1_LT__(a%first,b%first) .or. __T1_LT__(a%first,b%first))
#endif
      if (.not. equal) return
      
#if defined(__T2_EQ__)
      equal = __T2_EQ__(a%second,a%second)
#else
      equal = .not. (__T2_LT__(a%second,b%second) .or. __T1_LT__(a%second,b%second))
#endif

      return
   end function __MANGLE(equal)


   ! =======================
   !  not_equal
   ! =======================
   logical function __MANGLE(not_equal)(a, b) result(not_equal)
      type(__pair), intent(in) :: a
      type(__pair), intent(in) :: b

      not_equal = .not. (a == b)

      return
   end function __MANGLE(not_equal)
#endif

#if defined(__T1_LT__) && defined(__T2_LT__)
   ! ======================
   !  operator(<) less_than()
   ! ======================
   logical function __MANGLE(less_than)(a, b) result(lt)
      type(__pair), intent(in) :: a
      type(__pair), intent(in) :: b

      lt = __T1_LT__(a%first, b%first)

      if (lt) return
      lt = .not. (__T1_LT__(b%first, a%first)) .and. (__T2_LT__(a%second, b%second))
      
   end function __MANGLE(less_than)


   ! ======================
   !  operator(<=) - less_than_or_equal() 
   ! ======================
   logical function __MANGLE(less_than_or_equal)(a,b) result(le)
      type(__pair), intent(in) :: a
      type(__pair), intent(in) :: b

      le = .not. (b < a)
      return
   end function __MANGLE(less_than_or_equal)

   ! ======================
   !  operator(>) - greater_than()
   ! ======================
   logical function __MANGLE(greater_than)(a,b) result(gt)
      type(__pair), intent(in) :: a
      type(__pair), intent(in) :: b

      gt = (b < a)
      return
   end function __MANGLE(greater_than)

   ! ======================
   !  operator(>=) - greater_than_or_equal() 
   ! ======================
   logical function __MANGLE(greater_than_or_equal)(a,b) result(ge)
      type(__pair), intent(in) :: a
      type(__pair), intent(in) :: b

      ge = .not. (a < b)
      return
   end function __MANGLE(greater_than_or_equal)

#endif


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


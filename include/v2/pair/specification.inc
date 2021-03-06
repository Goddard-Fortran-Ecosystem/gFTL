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

   type :: __pair
      __T1_declare_component__ :: first
      __T2_declare_component__ :: second
   contains
   end type __pair

   interface __pair
      module procedure __MANGLE(new_pair)
   end interface __pair

#if (defined(__T1_LT__) || defined(__T1_EQ__)) && (defined(__T2_LT__) || defined(__T2_EQ__))
   interface operator(==)
      module procedure __MANGLE(equal)
   end interface operator(==)
   interface operator(/=)
      module procedure __MANGLE(not_equal)
   end interface operator(/=)
#endif
#if defined(__T1_LT__) && defined(__T2_LT__)
   interface operator(<)
      module procedure __MANGLE(less_than)
   end interface operator(<)
   interface operator(<=)
      module procedure __MANGLE(less_than_or_equal)
   end interface operator(<=)
   interface operator(>)
      module procedure __MANGLE(greater_than)
   end interface operator(>)
   interface operator(>=)
      module procedure __MANGLE(greater_than_or_equal)
   end interface operator(>=)
#endif
   interface swap
      module procedure __MANGLE(swap)
   end interface swap


! undef derived template parameters
#include "parameters/T1/undef_derived_macros.inc"
#include "parameters/T2/undef_derived_macros.inc"

! undef internal template parameters
#undef __guard
#include "parameters/T1/undef_internal.inc"
#include "parameters/T2/undef_internal.inc"

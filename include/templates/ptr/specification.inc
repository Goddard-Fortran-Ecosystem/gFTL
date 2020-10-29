!--------------------------------------------------------------------
! Copyright © 2020 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------


! define generic internal template parameters from pair parameters.
#include "parameters/T/copy_ptr_T_to_internal_T.inc"
#define __guard __ptr_guard

! define derived generic template parameters from internal parameters.
#include "parameters/T/define_derived_macros.inc"

   type :: __ptr
      private
      __T_declare_component__, pointer :: ptr
   contains
!!$      procedure :: write_formatted => __MANGLE(get)
      procedure :: get => __MANGLE(get)
   end type __ptr

   interface __ptr
      module procedure __MANGLE(new_ptr)
   end interface __ptr

   interface operator(==)
      module procedure __MANGLE(equal)
   end interface operator(==)
   interface operator(/=)
      module procedure __MANGLE(not_equal)
   end interface operator(/=)

   interface swap
      module procedure __MANGLE(swap)
   end interface swap


! undef derived template parameters
#include "parameters/T/undef_derived_macros.inc"

! undef internal template parameters
#undef __guard
#include "parameters/T/undef_internal.inc"
   

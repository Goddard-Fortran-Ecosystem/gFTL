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
! containers.  E.g., Set uses Deque and Map uses Pair.
!---------------

#include "parameters/T/copy_stack_T_to_deque_T.inc"

#define __deque __stack_container_type
#define __deque_guard __IDENTITY(__stack_guard)__IDENTITY(d_)
   
#include "deque/specification.inc"

#undef __deque
#undef __deque_guard


#include "parameters/T/copy_stack_T_to_internal_T.inc"
#define __guard __stack_guard

! define derived generic template parameters from internal parameters.
#include "parameters/T/define_derived_macros.inc"

   
   ! Structure constructors
   interface __stack
      module procedure __MANGLE(new_stack_empty)
      module procedure __MANGLE(new_stack_container)
      module procedure __MANGLE(new_stack_copy)
   end interface __stack

   type :: __stack
      private
      type(__stack_container_type) :: container
   contains

      procedure :: size => __MANGLE(size)
      procedure :: empty => __MANGLE(empty)
!      procedure, nopass :: max_size => __MANGLE(max_size)
!      procedure :: capacity => __MANGLE(capacity)

      ! Accessors

      procedure :: top => __MANGLE(top)

      procedure :: push => __MANGLE(push)
      procedure :: pop => __MANGLE(pop)

      procedure :: swap => __MANGLE(swap)

      ! I/O
      procedure :: write_formatted => __MANGLE(write_formatted)
      generic :: write(formatted) => write_formatted

   end type __stack


   interface swap
      module procedure __MANGLE(swap)
   end interface swap


#if defined(__T_EQ__) || defined(__T_LT__)
   interface operator(==)
      module procedure __MANGLE(equal)
   end interface operator(==)
   interface operator(/=)
      module procedure __MANGLE(not_equal)
   end interface operator(/=)

#endif

#ifdef __T_LT__
   interface operator(<)
      module procedure __MANGLE(less_than)
   end interface
   interface operator(>)
      module procedure __MANGLE(greater_than)
   end interface
   interface operator(<=)
      module procedure __MANGLE(less_than_or_equal_to)
   end interface
   interface operator(>=)
      module procedure __MANGLE(greater_than_or_equal_to)
   end interface
#endif
      



! undef derived template parameters
#include "parameters/T/undef_derived_macros.inc"

! undef internal template parameters
#undef __guard
#include "parameters/T/undef_internal.inc"


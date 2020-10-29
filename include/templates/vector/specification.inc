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
#include "parameters/T/copy_vector_T_to_internal_T.inc"
#define __guard __vector_guard

! define derived generic template parameters from internal parameters.
#include "parameters/T/define_derived_macros.inc"
   
   ! Structure constructors
   interface __vector
      module procedure __MANGLE(new_vector_empty)
#ifndef  __gftl_disable_index_kind_overload
      module procedure __MANGLE(new_vector_fill_size_kind)
#endif
      module procedure __MANGLE(new_vector_fill_default)
      module procedure __MANGLE(new_vector_range)
      module procedure __MANGLE(new_vector_copy)
      ! module procedure __MANGLE(new_vector_move) ! not supported

#ifdef __T_listable__
      module procedure __MANGLE(new_vector_initializer_list)
#endif
   end interface __vector

   ! Internal wrapper type allows support for types that cannot be
   ! directly treated as arrays: arrays, deferred type, deferred
   ! length.  To reduce implementation complexity the wrapper is used
   ! even when technically not required.  
   type :: __MANGLE(wrapper)
      __T_declare_component__ :: item
   end type __MANGLE(Wrapper)
   
   type :: __vector
      private
      type(__MANGLE(wrapper)), allocatable :: elements(:)
      integer(kind=GFTL_SIZE_KIND) :: vsize = 0
   contains

      procedure :: size => __MANGLE(size)
      procedure, nopass :: max_size => __MANGLE(max_size)
      procedure :: capacity => __MANGLE(capacity)
      procedure :: empty => __MANGLE(empty)

      ! Accessors
      procedure :: at_default => __MANGLE(at_default)
      procedure :: of_default => __MANGLE(of_default)
      generic :: at => at_default
      generic :: at => of_default
      ! Legacy interface from V1 - may be deleted in V3
      generic :: of => of_default

#ifndef  __gftl_disable_index_kind_overload
      procedure :: at_size_kind => __MANGLE(at_size_kind)
      procedure :: of_size_kind => __MANGLE(of_size_kind)
      generic :: at => at_size_kind
      generic :: at => of_size_kind
      generic :: of => of_size_kind
#endif

#if !defined(__T_allocatable__) && !(__T_rank__ > 0)
      procedure :: data => __MANGLE(data)
#endif
      procedure :: back => __MANGLE(back)
      procedure :: front => __MANGLE(front)

      procedure :: set_size_kind => __MANGLE(set_size_kind)
      procedure :: set_default => __MANGLE(set_default)
      generic :: set => set_default
#ifndef  __gftl_disable_index_kind_overload
      generic :: set => set_size_kind
#endif

#if !defined(__T_allocatable__) && !(__T_rank__ > 0)
      procedure :: copy_from_array => __MANGLE(copy_from_array)
      generic :: assignment(=) => copy_from_array
#endif
      procedure :: push_back => __MANGLE(push_back)
      procedure :: pop_back => __MANGLE(pop_back)

      procedure :: resize_size_kind => __MANGLE(resize_size_kind)
      procedure :: resize_default => __MANGLE(resize_default)
      generic :: resize => resize_default
#ifndef  __gftl_disable_index_kind_overload
      generic :: resize => resize_size_kind
#endif
      

      procedure :: clear => __MANGLE(clear)
      procedure :: shrink_to_fit => __MANGLE(shrink_to_fit)

      procedure :: erase_one => __MANGLE(erase_one)
      procedure :: erase_range => __MANGLE(erase_range)
      generic :: erase => erase_one
      generic :: erase => erase_range

      procedure :: swap => __MANGLE(swap)
      procedure :: reset => __MANGLE(reset)

      procedure :: reserve_size_kind => __MANGLE(reserve_size_kind)
      procedure :: reserve_default => __MANGLE(reserve_default)
      generic :: reserve => reserve_default
#ifndef  __gftl_disable_index_kind_overload
      generic :: reserve => reserve_size_kind
#endif


      ! Factory methods for iterators
      procedure :: begin => __MANGLE(begin)
      procedure :: end => __MANGLE(end)
      procedure :: rbegin => __MANGLE(rbegin)
      procedure :: rend => __MANGLE(rend)

      ! No analog for the following in STL, but it seems very convenient.
#ifdef __T_EQ__
      procedure :: get_index => __MANGLE(get_index)
#endif

      ! private helper procedures
      procedure, private :: set_capacity => __MANGLE(set_capacity)
      procedure, private :: grow_to => __MANGLE(grow_to)
      procedure, private :: downsize=>__MANGLE(downsize)
   end type __vector


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
      

#include "templates/vector/iterator_specification.inc"
#include "templates/vector/reverse_iterator_specification.inc"


! undef derived template parameters
#include "parameters/T/undef_derived_macros.inc"

! undef internal template parameters
#undef __guard
#include "parameters/T/undef_internal.inc"


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

#include "parameters/T/copy_deque_T_to_internal_T.inc"
#define __guard __deque_guard

#ifdef __deque_iterator
#    define __deque_iterator__ __deque_iterator
#else
#    define __deque_iterator__ DequeIterator
#endif

#ifdef __deque_riterator
#    define __deque_riterator__ __deque_riterator
#else
#    define __deque_riterator__ DequeRIterator
#endif

! define derived generic template parameters from internal parameters.
#include "parameters/T/define_derived_macros.inc"
   
   ! Structure constructors
   interface __deque
      module procedure __MANGLE(new_deque_empty)
#ifndef  __gftl_disable_index_kind_overload
      module procedure __MANGLE(new_deque_fill_size_kind)
#endif
      module procedure __MANGLE(new_deque_fill_default)
      module procedure __MANGLE(new_deque_range)
      module procedure __MANGLE(new_deque_copy)
!!$      module procedure __MANGLE(new_deque_move)

   end interface __deque

   ! Internal wrapper type allows support for types that cannot be
   ! directly treated as arrays: arrays, deferred type, deferred
   ! length.  To reduce implementation complexity the wrapper is used
   ! even when technically not required.  
   type :: __MANGLE(wrapper)
      __T_declare_component__ :: item
   end type __MANGLE(Wrapper)

   ! "T_rank"   T_rank __T_rank __T_rank__
   ! "T_shape"  T_shape __T_shape __T_shape__
   ! "component" __T_declare_component__
   ! "dimension" __T_dimension_component__
   ! "default" __T_default__

   ! Note: Bucket size is not yet implemented.
   type :: __MANGLE(bucket)
      type(__MANGLE(wrapper)) :: bucket_items
   end type __MANGLE(bucket)

   type :: __MANGLE(bucket_ptr)
      type(__MANGLE(bucket)), pointer :: ptr => null()
   end type __MANGLE(bucket_ptr)

   type :: __deque
      private
      type(__MANGLE(bucket_ptr)), allocatable :: buckets(:)
      integer(kind=GFTL_SIZE_KIND) :: size_ = 0
      integer(kind=GFTL_SIZE_KIND) :: front_ = -1
      integer(kind=GFTL_SIZE_KIND) :: back_ = 0
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

      procedure :: back => __MANGLE(back)
      procedure :: front => __MANGLE(front)

      procedure :: set_size_kind => __MANGLE(set_size_kind)
      procedure :: set_default => __MANGLE(set_default)
      generic :: set => set_default
#ifndef  __gftl_disable_index_kind_overload
      generic :: set => set_size_kind
#endif

      procedure :: push_back => __MANGLE(push_back)
      procedure :: pop_back => __MANGLE(pop_back)

      ! deques are not vectors ...
      procedure :: push_front => __MANGLE(push_front)
      procedure :: pop_front => __MANGLE(pop_front)

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
      ! I/O
      procedure :: write_formatted => __MANGLE(write_formatted)
      generic :: write(formatted) => write_formatted

      ! private helper procedures
      procedure, private :: set_capacity => __MANGLE(set_capacity)
      procedure, private :: grow_to => __MANGLE(grow_to)
      procedure, private :: downsize=>__MANGLE(downsize)
      procedure, private :: idx => __MANGLE(idx)
      procedure, private :: bucket_index => __MANGLE(bucket_index)
      procedure, private :: bucket_offset => __MANGLE(bucket_offset)
   end type __deque


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
      

#include "deque/iterator_specification.inc"
#include "deque/reverse_iterator_specification.inc"

#ifdef __deque_iterator__
#    undef __deque_iterator__
#endif

#ifdef __deque_riterator__
#    undef __deque_riterator__
#endif

! undef derived template parameters
#include "parameters/T/undef_derived_macros.inc"

! undef internal template parameters
#undef __guard
#include "parameters/T/undef_internal.inc"


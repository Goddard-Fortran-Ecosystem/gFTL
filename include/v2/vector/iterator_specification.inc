!--------------------------------------------------------------------
! Copyright © 2020 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

   type :: __vector_iterator
      private

      type(__MANGLE(wrapper)), dimension(:), pointer :: elements
      integer(kind=__gftl_size_kind) :: current_index = -1 ! intentionally invalid value

   contains

      procedure :: next => __MANGLE(iter_next)
      procedure :: prev => __MANGLE(iter_prev)

!!$      procedure :: back_inserter => __MANGLE(back_inserter)
!!$      procedure :: front_inserter => __MANGLE(front_inserter)
!!$      procedure :: inserter => __MANGLE(inserter)
!!$      procedure :: make_move_iterator => __MANGLE(make_move_iterator)

!!$      procedure :: insert_size_kind => __MANGLE(insert_size_kind)
!!$      procedure :: insert_default => __MANGLE(insert_default)
!!$      generic :: insert => insert_default
!!$#ifndef  __gftl_disable_index_kind_overload
!!$      generic :: insert => insert_size_kind
!!$#endif

      procedure :: __MANGLE(iter_of)
      procedure :: __MANGLE(iter_of_offset_size_kind)
      procedure :: __MANGLE(iter_of_offset_default)
      generic :: of => __MANGLE(iter_of)
      generic :: of => __MANGLE(iter_of_offset_default)
#ifndef  __gftl_disable_index_kind_overload
      generic :: of => __MANGLE(iter_of_offset_size_kind)
#endif

      ! Anolog of STL "+=" and "-=" operators
      procedure :: __MANGLE(iter_add_n_size_kind)
      procedure :: __MANGLE(iter_sub_n_size_kind)
      procedure :: __MANGLE(iter_add_n_default)
      procedure :: __MANGLE(iter_sub_n_default)
      generic :: add => __MANGLE(iter_add_n_default)
      generic :: sub => __MANGLE(iter_sub_n_default)
#ifndef  __gftl_disable_index_kind_overload
      generic :: add => __MANGLE(iter_add_n_size_kind)
      generic :: sub => __MANGLE(iter_sub_n_size_kind)
#endif

   end type __vector_iterator


   ! Non type-bound functions

   ! Arithmetic
   interface operator(+)
#ifndef  __gftl_disable_index_kind_overload
      module procedure __MANGLE(iter_a_plus_n_size_kind)
      module procedure __MANGLE(iter_n_plus_a_size_kind)
#endif
      module procedure __MANGLE(iter_n_plus_a_default)
      module procedure __MANGLE(iter_a_plus_n_default)
   end interface operator(+)
   interface operator(-)
#ifndef  __gftl_disable_index_kind_overload
      module procedure __MANGLE(iter_a_minus_n_size_kind)
#endif
      module procedure __MANGLE(iter_a_minus_n_default)
      module procedure __MANGLE(iter_a_minus_b)
   end interface operator(-)


   ! Relational operators
   interface operator(==)
      module procedure __MANGLE(iter_equal)
   end interface operator(==)

   interface operator(/=)
      module procedure __MANGLE(iter_not_equal)
   end interface operator(/=)

   interface operator (<)
      module procedure __MANGLE(iter_less)
   end interface operator (<)

   interface operator (<=)
      module procedure __MANGLE(iter_less_equal)
   end interface operator (<=)

   interface operator (>)
      module procedure __MANGLE(iter_greater)
   end interface operator (>)

   interface operator (>=)
      module procedure __MANGLE(iter_greater_equal)
   end interface operator (>=)

   interface advance
#ifndef  __gftl_disable_index_kind_overload
      module procedure __MANGLE(iter_advance_size_kind)
#endif
      module procedure __MANGLE(iter_advance_default)
   end interface advance

   interface begin
      module procedure __MANGLE(iter_begin)
   end interface begin

   interface ftn_begin
      module procedure __MANGLE(iter_ftn_begin)
   end interface ftn_begin

   interface distance
      module procedure __MANGLE(iter_distance)
   end interface distance

   interface end
      module procedure __MANGLE(iter_end)
   end interface end

   interface ftn_end
      module procedure __MANGLE(iter_ftn_end)
   end interface ftn_end

   interface next
      ! Cannot use optional dummy due to otherwise ambiguous interfaces
      module procedure __MANGLE(iter_next_1)
#ifndef  __gftl_disable_index_kind_overload
      module procedure __MANGLE(iter_next_n_size_kind)
#endif
      module procedure __MANGLE(iter_next_n_default)
   end interface next

   interface prev
      module procedure __MANGLE(iter_prev_1)
#ifndef __gftl_disable_index_kind_overload
      module procedure __MANGLE(iter_prev_n_size_kind)
#endif
      module procedure __MANGLE(iter_prev_n_default)
   end interface prev

!--------------------------------------------------------------------
! Copyright © 2020 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------



#if (defined(__guard))
#  undef __guard
#endif
#define __guard __alt_set_guard

#include "parameters/T/copy_alt_set_T_to_internal_T.inc"
#include "parameters/T/define_derived_macros.inc"

#define __LEFT 0
#define __RIGHT 1

   ! Structure constructors
   interface __alt_set
      module procedure __MANGLE(new_set_empty)
      module procedure __MANGLE(new_set_copy)

#ifdef __T_listable__
      module procedure __MANGLE(new_set_initializer_list)
#endif
   end interface __alt_set

! Vector spec   
#define __vector __MANGLE(Vector)
#define __vector_iterator ItemVectorIterator
#define __vector_riterator ItemVectorRIterator
#define __vector_T __alt_set_T
#define __vector_guard __IDENTITY(__alt_set_guard)__IDENTITY(Item)

#include "vector/specification.inc"

#undef __vector_guard
#undef __vector_T
#undef __vector_riterator
#undef __vector_iterator
#undef __vector

#define __vector __MANGLE(Vector)
#define __vector_iterator IndexVectorIterator
#define __vector_riterator IndexVectorRIterator
#define __vector_T __INTEGER64
#define __vector_guard __IDENTITY(__alt_set_guard)__IDENTITY(Index)

#include "vector/specification.inc"

#undef __vector_guard
#undef __vector_T
#undef __vector_iterator
#undef __vector_riterator
#undef __vector

#include "parameters/T/copy_alt_set_T_to_internal_T.inc"
#include "parameters/T/define_derived_macros.inc"

#define __guard __alt_set_guard

   integer(kind=GFTL_SIZE_KIND), parameter :: UNINITIALIZED = -1

   type :: __alt_set
      private
      integer(kind=GFTL_SIZE_KIND) :: root = UNINITIALIZED
      integer(kind=GFTL_SIZE_KIND) :: tsize = 0
      integer(kind=GFTL_SIZE_KIND) :: next_free = 0

      type(__MANGLE(ItemVector)) :: items
      type(__MANGLE(IndexVector)) :: parents
      type(__MANGLE(IndexVector)) :: lefts
      type(__MANGLE(IndexVector)) :: rights
      type(__MANGLE(IndexVector)) :: heights
   contains
      procedure :: empty => __MANGLE(empty)
      procedure :: size => __MANGLE(size)
      procedure, nopass :: max_size => __MANGLE(max_size)
      procedure :: count => __MANGLE(count)
      procedure :: find => __MANGLE(find)
      procedure :: clear => __MANGLE(clear)
      
      procedure :: insert_single => __MANGLE(insert_single)
      procedure :: insert_single_with_hint => __MANGLE(insert_single_with_hint)
      procedure :: insert_range => __MANGLE(insert_range)
#ifdef __T_listable__
      procedure :: insert_initializer_list => __MANGLE(insert_initializer_list)
#endif
      generic :: insert => insert_single
      generic :: insert => insert_single_with_hint
      generic :: insert => insert_range
#ifdef __T_listable__
      generic :: insert => insert_initializer_list
#endif

      procedure :: erase_iter => __MANGLE(erase_iter)
      procedure :: erase_value => __MANGLE(erase_value)
      procedure :: erase_range => __MANGLE(erase_range)
      generic :: erase => erase_iter, erase_value, erase_range
      procedure :: begin => __MANGLE(begin)
      procedure :: end => __MANGLE(end)
      procedure :: ftn_begin => __MANGLE(ftn_begin)
      procedure :: ftn_end => __MANGLE(ftn_end)
      procedure :: lower_bound => __MANGLE(lower_bound)
      procedure :: upper_bound => __MANGLE(upper_bound)

      procedure :: merge  => __MANGLE(merge)

      procedure :: deep_copy => __MANGLE(deep_copy)
      
#ifdef __T_listable__
      procedure :: copy_list => __MANGLE(copy_list)
      generic :: assignment(=) => copy_list
#endif

      procedure :: swap => __MANGLE(swap)

      procedure, private :: rebalance=> __MANGLE(rebalance)
      procedure, private :: erase_nonleaf => __MANGLE(erase_nonleaf)
      procedure, private :: advpos => __MANGLE(advpos)
      procedure, private :: rot => __MANGLE(rot)
      procedure, private :: find_index => __MANGLE(find_index)
      procedure, private :: set_child => __MANGLE(set_child)
      procedure, private :: get_child => __MANGLE(get_child)
      procedure, private :: set_parent_child => __MANGLE(set_parent_child)
      procedure, private :: update_height => __MANGLE(update_height)

      ! I/O
      procedure :: write_formatted => __MANGLE(write_formatted)
      generic :: write(formatted) => write_formatted

      ! key_compare() and value_compare() are the same thing for Set.
      procedure :: key_compare => __MANGLE(value_compare)
      procedure :: value_compare => __MANGLE(value_compare)
   end type __alt_set

   interface swap
      module procedure __MANGLE(swap)
   end interface swap

   interface operator(==)
      module procedure __MANGLE(equal)
   end interface operator(==)
   interface operator(/=)
      module procedure __MANGLE(not_equal)
   end interface operator(/=)
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


#include "alt_set/iterator_specification.inc"


#undef __LEFT
#undef __RIGHT
#undef __alt_set_node

! undef derived template parameters
#include "parameters/T/undef_derived_macros.inc"
! undef internal template parameters
#undef __guard
#include "parameters/T/undef_internal.inc"

#include "parameters/T/copy_alt_set_T_to_algorithm_T.inc"
#define __algorithm_guard __IDENTITY(__alt_set_guard)
#define __algorithm_container __alt_set
#define __algorithm_iterator __alt_set_iterator
#include "algorithms/specification.inc"
#undef __algorithm_container
#undef __algorithm_iterator
#undef __algorithm_guard
#include "parameters/T/undef_algorithm_T.inc"

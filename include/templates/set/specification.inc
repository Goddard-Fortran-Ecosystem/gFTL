!--------------------------------------------------------------------
! Copyright © 2020 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

#include "templates/set/stash_T.inc"

#define __T __INTEGER
#define __vector_guard  __IDENTITY(__set_guard)__IDENTITY(i_)
#define __vector __IDENTITY(__set_guard)__IDENTITY(Vector)
#define __vector_iterator __IDENTITY(__set_guard)__IDENTITY(VectorIterator)
#define __vector_riterator __IDENTITY(__set_guard)__IDENTITY(VectorRIterator)

#include "templates/vector/specification.inc"

#undef __vector
#undef __vector_iterator
#undef __vector_riterator
#undef __vector_guard
#undef __T   

   __set_T
#include "templates/set/restore_T.inc"
   __set_T

#define __vector_guard __IDENTITY(__set_guard)__IDENTITY(T_)
#define __vector __MANGLE(Vector)
#define __vector_iterator __MANGLE(VectorIterator)
#define __vector_riterator __MANGLE(VectorRIterator)

#include "templates/vector/specification.inc"

#undef __vector
#undef __vector_iterator
#undef __vector_riterator
#undef __vector_guard



#define __guard __set_guard
#include "parameters/T/define_derived_macros.inc"

#define __UNINITIALIZED -1
#define __LEFT 0
#define __RIGHT 1

   ! Structure constructors
   interface __set
      module procedure __MANGLE(new_set_empty)
!!$      module procedure __MANGLE(new_set_range)
      module procedure __MANGLE(new_set_copy)
!!$      module procedure __MANGLE(new_set_move)

#ifdef __T_listable__
      module procedure __MANGLE(new_set_initializer_list)
#endif
   end interface __set


   type :: __set
      private
      type (Tvector) :: items
      type (Ivector) :: parents
      type (Ivector) :: lefts
      type (Ivector) :: rights
      type (Ivector) :: heights
      integer(kind=__gftl_size_kind) :: root = __UNINITIALIZED
      integer(kind=__gftl_size_kind) :: tsize = 0
      integer(kind=__gftl_size_kind) :: next_free = 0

   contains

      procedure, nopass :: max_size => __MANGLE(max_size)
      procedure :: empty => __MANGLE(empty)
      procedure :: size => __MANGLE(size)
      procedure :: count => __MANGLE(count)
      procedure :: find => __MANGLE(find)
      procedure :: clear => __MANGLE(clear)
      procedure :: insert => __MANGLE(insert)

      procedure :: erase_one => __MANGLE(erase_one)
      procedure :: erase_multi => __MANGLE(erase_multi)
      generic :: erase => erase_one, erase_multi

      procedure :: remove => __MANGLE(remove)
      procedure :: begin => __MANGLE(begin)
      procedure :: end => __MANGLE(end)
      procedure :: lower_bound => __MANGLE(lower_bound)
      procedure :: upper_bound => __MANGLE(upper_bound)
      procedure :: dump => __MANGLE(dump)
      procedure :: deepCopy => __MANGLE(deepCopy)

      ! key_compare() and value_compare() are the same thing for Set.
      procedure :: key_compare => __MANGLE(value_compare)
      procedure :: value_compare => __MANGLE(value_compare)

      procedure, private :: get_child => __MANGLE(get_child)
      procedure, private :: set_child => __MANGLE(set_child)
      procedure, private :: set_parent_child => __MANGLE(set_parent_child)
      procedure, private :: find_index => __MANGLE(find_index)
      procedure, private :: update_height => __MANGLE(update_height)
      procedure, private :: rebalance => __MANGLE(rebalance)
      procedure, private :: erase_nonleaf => __MANGLE(erase_nonleaf)
      procedure, private :: advpos => __MANGLE(advpos)
      procedure, private :: rot => __MANGLE(rot)
      ! Note - mangling makes this helper obscure unless we make it
      ! type-bound.
      procedure, nopass :: exists => __MANGLE(exists)
   end type __set


   interface operator(==)
      module procedure __MANGLE(equal)
   end interface operator(==)
   interface operator(==)
      module procedure __MANGLE(not_equal)
   end interface operator(==)

   interface merge
      module procedure __MANGLE(merge)
   end interface merge


#include "templates/set/iterator_specification.inc"
   !#include "templates/set/reverse_iterator_specification.inc"


#undef __UNINITIALIZED -1
#undef __LEFT 0
#undef __RIGHT 1

#undef __guard

#include "parameters/T/undef_derived_macros.inc"


!--------------------------------------------------------------------
! Copyright © 2020 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

#define __vector_T integer
#define __vector_T_name "integer" 
#define __vector_T_KINDLEN(context) (kind=__gftl_size_kind)
#define __vector_T_kindlen_string "(kind=__gftl_size_kind)"

#define __vector_guard  __IDENTITY(__set_guard)__IDENTITY(I_)
#define __vector __IDENTITY(__vector_guard)__IDENTITY(Vector)
#define __vector_iterator __IDENTITY(__vector_guard)__IDENTITY(VectorIterator)
#define __vector_riterator __IDENTITY(__vector_guard)__IDENTITY(VectorRIterator)

#include "templates/vector/specification.inc"

#undef __vector
#undef __vector_iterator
#undef __vector_riterator
#undef __vector_guard
#include "parameters/T/undef_vector_T.inc"


#include "parameters/T/copy_set_T_to_vector_T.inc"
#define __vector_guard __IDENTITY(__set_guard)__IDENTITY(T_)
#define __vector __IDENTITY(__vector_guard)__IDENTITY(Vector)
#define __vector_iterator __IDENTITY(__vector_guard)__IDENTITY(VectorIterator)
#define __vector_riterator __IDENTITY(__set_guard)__IDENTITY(T_VectorRIterator)

#include "templates/vector/specification.inc"

#undef __vector
#undef __vector_iterator
#undef __vector_riterator
#undef __vector_guard
#include "parameters/T/undef_vector_T.inc"



#define __guard __set_guard
#include "parameters/T/copy_set_T_to_internal_T.inc"
#include "parameters/T/define_derived_macros.inc"

#define __UNINITIALIZED int(-1,kind=__gftl_size_kind)
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
      type (__MANGLE(T_Vector)) :: items
      type (__MANGLE(I_Vector)) :: parents
      type (__MANGLE(I_Vector)) :: lefts
      type (__MANGLE(I_Vector)) :: rights
      type (__MANGLE(I_Vector)) :: heights
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
      procedure :: insert_single => __MANGLE(insert_single)
      procedure :: insert_range => __MANGLE(insert_range)
#ifdef __T_listable__
      procedure :: insert_initializer_list => __MANGLE(insert_initializer_list)
#endif
      generic :: insert => insert_single
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
      procedure :: lower_bound => __MANGLE(lower_bound)
      procedure :: upper_bound => __MANGLE(upper_bound)

      procedure :: merge  => __MANGLE(merge)
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
   interface operator(/=)
      module procedure __MANGLE(not_equal)
   end interface operator(/=)


#include "templates/altset/iterator_specification.inc"
   !#include "templates/altset/reverse_iterator_specification.inc"


#undef __UNINITIALIZED -1
#undef __LEFT 0
#undef __RIGHT 1

! undef derived template parameters
#include "parameters/T/undef_derived_macros.inc"
! undef internal template parameters
#undef __guard
#include "parameters/T/undef_internal.inc"


!--------------------------------------------------------------------
! Copyright © 2020 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------



#define __guard __set_guard
#include "parameters/T/copy_set_T_to_internal_T.inc"
#include "parameters/T/define_derived_macros.inc"

#define __LEFT 0
#define __RIGHT 1
#define __set_node __MANGLE(Node)

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

   type :: __set_node
      type(__set_node), pointer :: parent => null()
      type(__set_node), pointer :: left => null()
      type(__set_node), pointer :: right => null()
      integer :: height=1
      __T_declare_component__ :: value
   contains
      procedure :: child => __MANGLE(child)
      procedure :: which_child => __MANGLE(which_child)
      procedure :: set_child => __MANGLE(set_child)
      procedure, private :: update_height => __MANGLE(update_height)
      final :: __MANGLE(finalize_node)
   end type __set_node
      

   type :: __set
      type(__set_node), pointer :: root => null()
      integer(kind=GFTL_SIZE_KIND) :: tsize = 0
   contains
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

      procedure :: deep_copy => __MANGLE(deep_copy)
      generic :: assignment(=) => deep_copy
#ifdef __T_listable__
      procedure :: copy_list => __MANGLE(copy_list)
      generic :: assignment(=) => copy_list
#endif

      procedure, private :: find_node => __MANGLE(find_node)
      procedure, private :: rebalance=> __MANGLE(rebalance)
      procedure, private :: erase_nonleaf => __MANGLE(erase_nonleaf)
      procedure, private :: advpos => __MANGLE(advpos)
      procedure, private :: rot => __MANGLE(rot)

      ! key_compare() and value_compare() are the same thing for Set.
      procedure :: key_compare => __MANGLE(value_compare)
      procedure :: value_compare => __MANGLE(value_compare)
      final :: __MANGLE(finalize_set)

   end type __set


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


#include "set/iterator_specification.inc"
!#include "set/reverse_iterator_specification.inc"


#undef __LEFT 0
#undef __RIGHT 1
#undef __set_node

! undef derived template parameters
#include "parameters/T/undef_derived_macros.inc"
! undef internal template parameters
#undef __guard
#include "parameters/T/undef_internal.inc"


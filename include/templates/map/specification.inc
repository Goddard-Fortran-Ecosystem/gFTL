!--------------------------------------------------------------------
! Copyright © 2020 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

#include "parameters/T1/copy_map_Key_to_pair_T1.inc"
#include "parameters/T2/copy_map_T_to_pair_T2.inc"

#define __pair __map_pair
#define __pair_guard __IDENTITY(__map_guard)__IDENTITY(p_)
   
#include "templates/pair/specification.inc"

#undef __pair
#undef __pair_guard

#include "parameters/T1/undef_pair_T1.inc"   
#include "parameters/T2/undef_pair_T2.inc"   


#define __set __IDENTITY(__map_guard)__IDENTITY(__map_set)
#define __set_iterator __IDENTITY(__map_guard)__IDENTITY(__map_set_iterator)
#define __set_T __map_pair
#define __set_T_LT(a,b) ((a) < (b))
#define __set_guard __IDENTITY(__map_guard)__IDENTITY(s_)

#include "templates/set/specification.inc"

#undef __set
#undef __set_iterator
#undef __set_guard
#include "parameters/T/undef_set_T.inc"


#define __guard __map_guard

#include "parameters/Key/copy_map_Key_to_internal_Key.inc"
#include "parameters/T/copy_map_T_to_internal_T.inc"
#include "parameters/Key/define_derived_macros.inc"
#include "parameters/T/define_derived_macros.inc"

   interface __map
      module procedure __MANGLE(new_map_empty)
      module procedure __MANGLE(new_map_copy)
      module procedure __MANGLE(new_map_initializer_list)
   end interface __map

   type :: __map
      private
      type(__MANGLE(Set)) :: tree
   contains
      procedure :: empty => __MANGLE(empty)
      procedure :: size => __MANGLE(size)
      procedure, nopass :: max_size => __MANGLE(max_size)

      procedure :: insert_key_value => __MANGLE(insert_key_value)
      procedure :: insert_pair => __MANGLE(insert_pair)
      generic :: insert => insert_key_value
      generic :: insert => insert_pair


      procedure :: of => __MANGLE(of) ! [] operator 
      procedure :: at_rc => __MANGLE(at_rc)
      generic :: at => of
      generic :: at => at_rc  ! [] operator

      procedure :: erase_iter => __MANGLE(erase_iter)
      procedure :: erase_key => __MANGLE(erase_key)
      procedure :: erase_range => __MANGLE(erase_range)
      generic :: erase => erase_iter
      generic :: erase => erase_key
      generic :: erase => erase_range
      procedure :: clear => __MANGLE(clear)
      procedure :: get => __MANGLE(get)
      procedure :: set => __MANGLE(set_)

      procedure :: begin => __MANGLE(begin)
      procedure :: end => __MANGLE(end)
      procedure :: find => __MANGLE(find)

      procedure :: count => __MANGLE(count)
      procedure :: deep_copy => __MANGLE(deep_copy)
      generic :: assignment(=) => deep_copy
!#ifdef _DUMP_MAP
!      procedure :: dump => mapdump
!#endif
   end type __map


   interface operator(==)
      module procedure __MANGLE(equal)
   end interface operator(==)
   interface operator(/=)
      module procedure __MANGLE(not_equal)
   end interface operator(/=)
!!$   interface operator(<)
!!$      module procedure __MANGLE(less_than)
!!$   end interface operator(<)
!!$   interface operator(<=)
!!$      module procedure __MANGLE(less_than_or_equal)
!!$   end interface operator(<=)
!!$   interface operator(>)
!!$      module procedure __MANGLE(greater_than)
!!$   end interface operator(>)
!!$   interface operator(>=)
!!$      module procedure __MANGLE(greater_than_or_equal)
!!$   end interface operator(>=)

#include "templates/map/iterator_specification.inc"


   ! undef derived template parameters
#include "parameters/Key/undef_derived_macros.inc"
#include "parameters/T/undef_derived_macros.inc"
   ! undef internal template parameters
#undef __guard
#include "parameters/Key/undef_internal.inc"
#include "parameters/T/undef_internal.inc"


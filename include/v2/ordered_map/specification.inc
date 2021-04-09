!--------------------------------------------------------------------
! Copyright © 2020 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------


   ! Declare map<T,Key>
#include "parameters/Key/copy_omap_Key_to_map_Key.inc"
#include "parameters/T/copy_omap_T_to_map_T.inc"

#define __map __IDENTITY(__omap_guard)__IDENTITY(Map)
#define __map_iterator __IDENTITY(__omap_guard)__IDENTITY(MapIterator)
#define __map_pair __omap_pair

#define __map_set __omap_set
#define __map_set_node __omap_set_node
#define __map_set_iterator __omap_set_iterator
#define __map_guard __IDENTITY(__omap_guard)__IDENTITY(I_)

#include "map/specification.inc"

#undef __map_set
#undef __map_set_node
#undef __map_set_iterator

#undef __map
#undef __map_pair
#undef __map_iterator
#undef __map_guard

#include "parameters/Key/undef_map_Key.inc"
#include "parameters/T/undef_map_T.inc"

   ! Declare vector<Key>
   ! __T   __T_declare_component__
#include "parameters/T/copy_omap_Key_to_vector_T.inc"
#define __vector_guard __IDENTITY(__omap_guard)__IDENTITY(K_)
#define __vector __IDENTITY(__omap_guard)__IDENTITY(Vector)
#define __vector_iterator __IDENTITY(__omap_guard)__IDENTITY(VectorIterator)
#define __vector_riterator __IDENTITY(__omap_guard)__IDENTITY(VectorRIterator)

#include "vector/specification.inc"

#undef __vector
#undef __vector_iterator
#undef __vector_riterator
#undef __vector_guard
#include "parameters/T/undef_vector_T.inc"


#define __guard __omap_guard

#include "parameters/Key/copy_omap_Key_to_internal_Key.inc"
#include "parameters/T/copy_omap_T_to_internal_T.inc"
#include "parameters/Key/define_derived_macros.inc"
#include "parameters/T/define_derived_macros.inc"

   interface __omap
      module procedure __MANGLE(new_omap_empty)
      module procedure __MANGLE(new_omap_copy)
      module procedure __MANGLE(new_omap_initializer_list)
   end interface __omap
      


   type :: __omap
      private
      type(__MANGLE(Map)) :: map
      type(__MANGLE(Vector)) :: ordered_keys
   contains
      procedure :: empty => __MANGLE(empty)
      procedure :: size => __MANGLE(size)
      procedure, nopass :: max_size => __MANGLE(max_size)

      procedure :: insert_key_value => __MANGLE(insert_key_value)
      ! TODO:  insert_pair is problematic
!!$      procedure :: insert_pair => __MANGLE(insert_pair)
      generic :: insert => insert_key_value
!!$      generic :: insert => insert_pair
!!$      procedure :: insert_or_assign => __MANGLE(insert_or_assign)


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
   end type __omap

   interface operator(==)
      module procedure __MANGLE(equal)
   end interface operator(==)
   interface operator(/=)
      module procedure __MANGLE(not_equal)
   end interface operator(/=)

#include "ordered_map/iterator_specification.inc"
   
   ! undef derived template parameters
#include "parameters/Key/undef_derived_macros.inc"
#include "parameters/T/undef_derived_macros.inc"
   ! undef internal template parameters
#undef __guard
#include "parameters/Key/undef_internal.inc"
#include "parameters/T/undef_internal.inc"

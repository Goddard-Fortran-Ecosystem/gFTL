! Main container file must define
!   __vector
!   __container_prefix
! Maiy also define
!   __iterator
!   __riterator

#include "templates/all_template_macros.inc"

#ifdef __type_wrapped
#  define __declare_element_type type(__PROC(Wrapper))
      type :: __PROC(Wrapper)
         __type_declare_component :: item
      end type __PROC(Wrapper)
#else
#  define __declare_element_type __type_declare_type
#endif


      type :: __vector
         private
         __declare_element_type, allocatable :: elements(:)
         integer(kind=SIZE_KIND) :: vsize = 0
      contains
         procedure :: size => __PROC(size)
         procedure :: capacity => __PROC(capacity)
         procedure :: empty => __PROC(empty)

         procedure :: at_size_kind => __PROC(at_size_kind)
         generic :: at => at_size_kind
#ifdef SUPPORT_FOR_INT64
         procedure :: at_32 => __PROC(at_32)
         generic :: at => at_32
#endif

         procedure :: of => __PROC(of)
         procedure :: get_size_kind => __PROC(get_size_kind)
         generic :: get => get_size_kind
#ifdef SUPPORT_FOR_INT64
         procedure :: get_32 => __PROC(get_32)
         generic :: get => get_32
#endif

#ifndef __type_wrapped
         procedure :: get_data => __PROC(get_data)
#endif
         procedure :: back => __PROC(back)
         procedure :: front => __PROC(front)


         procedure :: set_size_kind => __PROC(set_size_kind)
         generic :: set => set_size_kind
#ifdef SUPPORT_FOR_INT64
         procedure :: set_32 => __PROC(set_32)
         generic :: set => set_32
#endif

#ifndef __type_wrapped
         procedure :: copyFromArray => __PROC(copyfromarray)
         generic :: assignment(=) => copyFromArray
#endif
         procedure :: push_back => __PROC(push_back)
         procedure :: pop_back => __PROC(pop_back)
         procedure :: insert_size_kind => __PROC(insert_size_kind)
         generic :: insert => insert_size_kind
#ifdef SUPPORT_FOR_INT64
         procedure :: insert_32 => __PROC(insert_32)
         generic :: insert => insert_32
#endif

         procedure :: resize_size => __PROC(resize_size)
         generic :: resize => resize_size
#ifdef SUPPORT_FOR_INT64
         procedure :: resize_32 => __PROC(resize_32)
         generic :: resize => resize_32
#endif
         procedure :: clear => __PROC(clear)
         procedure :: shrink_to_fit => __PROC(shrink_to_fit)
#ifdef __iterator
         procedure :: __PROC(erase_one)
         procedure :: __PROC(erase_range)
         generic :: erase => __PROC(erase_one), __PROC(erase_range)
#endif

         procedure :: reserve_size_kind => __PROC(reserve_size_kind)
         generic :: reserve => reserve_size_kind
#ifdef SUPPORT_FOR_INT64
         procedure :: reserve_32 => __PROC(reserve_32)
         generic :: reserve => reserve_32
#endif


         procedure :: swap => __PROC(swap)
         procedure :: reset => __PROC(reset)
#ifdef __use_equal_defined
         procedure :: get_index => __PROC(get_index)
         procedure :: equal => __PROC(equal)
         procedure :: not_equal => __PROC(not_equal)
         generic :: operator(==) => equal
         generic :: operator(/=) => not_equal
#endif
#ifdef __type_compare_well_defined
         procedure, private :: diff=>__PROC(diff)
         procedure :: less_than => __PROC(less_than)
         procedure :: greater_than_or_equal_to =>                        &
     &      __PROC(greater_than_or_equal_to)
         generic :: operator(<) => less_than
         generic :: operator(>=) => greater_than_or_equal_to
         
         procedure :: greater_than => __PROC(greater_than)
         procedure :: less_than_or_equal_to =>                           &
     &      __PROC(less_than_or_equal_to)
         generic :: operator(>) => greater_than
         generic :: operator(<=) => less_than_or_equal_to
#endif

#ifdef __iterator
         ! Iterator constructors
         procedure :: begin => __PROC(begin)
         procedure :: end => __PROC(end)
#endif
         
#ifdef __iterator
         procedure :: rbegin => __PROC(rbegin)
         procedure :: rend => __PROC(rend)
#endif

         procedure, private :: set_capacity => __PROC(set_capacity)
         procedure, private :: grow_to => __PROC(grow_to)
         procedure, private :: downsize=>__PROC(downsize)
      end type __vector
      

      interface __vector
         module procedure __PROC(new_empty)
      end interface __vector

      interface swap
         module procedure __PROC(swap)
      end interface swap

#ifdef __iterator
#  include "vectorIterator_decl.inc"
#endif
#ifdef __riterator
#  include "vectorRIterator_decl.inc"
#endif

#undef __declare_element_type
#include "templates/all_template_macros_undefs.inc"

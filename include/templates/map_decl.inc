!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

#include "templates/all_template_macros.inc"

      type :: __map
         private
         type(__set) :: tree
      contains
         procedure :: empty => __PROC(empty)
         procedure :: size => __PROC(size)
         procedure, nopass :: max_size => __PROC(max_size)

         procedure :: insert_key_value => __PROC(insert_key_value)
         procedure :: insert_pair => __PROC(insert_pair)
         generic :: insert => insert_key_value
         generic :: insert => insert_pair

         procedure :: of => __PROC(of) ! [] operator
         procedure :: at => __PROC(at)
         procedure :: erase_one => __PROC(erase_one)
         generic :: erase => erase_one
         procedure :: clear => __PROC(clear)
         procedure :: get => __PROC(get)
         procedure :: set => __PROC(set)

         procedure :: begin => __PROC(begin)
         procedure :: end => __PROC(end)
         procedure :: find => __PROC(find)
         
         procedure :: count => __PROC(count)
         procedure :: deepCopy => __PROC(deepCopy)


#ifdef _DUMP_MAP
         procedure :: dump => mapdump
#endif
      end type __map


      type :: __iterator
        private
        type(__siterator) :: setIter
        class(__map), pointer :: reference
      contains
         procedure :: value => __PROC(value)
         procedure :: key => __PROC(key)
         procedure :: next => __PROC(next)
         procedure :: previous => __PROC(previous)
         procedure :: equal => __PROC(iter_equal)
         generic :: operator(==) => equal
         procedure :: notEqual => __PROC(iter_not_equal)
         generic :: operator(/=) => notEqual
      end type __iterator


#ifdef _alt
      interface __map
         module procedure __PROC(new_map_empty)
         module procedure __PROC(new_map_from_pair_array)
      end interface __map
#endif

#include "templates/all_template_macros_undefs.inc"

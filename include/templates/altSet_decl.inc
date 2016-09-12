#include "templates/all_template_macros.inc"

      integer(kind=SIZE_KIND), parameter :: UNINITIALIZED = -1

      integer, parameter :: LEFT = 0, RIGHT = 1

      type :: __set
        private
        type (Tvector) :: items
        type (Ivector) :: parents
        type (Ivector) :: lefts
        type (Ivector) :: rights
        type (Ivector) :: heights
        integer(kind=SIZE_KIND) :: root = UNINITIALIZED
        integer(kind=SIZE_KIND) :: tsize = 0
        integer(kind=SIZE_KIND) :: next_free = 0
      contains
        procedure :: empty => __PROC(empty)
        procedure :: size => __PROC(size)
        procedure :: count => __PROC(count)
        procedure :: find => __PROC(find)
        procedure :: clear => __PROC(clear)
        procedure :: insert => __PROC(insert)
        procedure :: erase_one => __PROC(erase_one)
        procedure :: erase_multi => __PROC(erase_multi)
        generic :: erase => erase_one, erase_multi
        procedure :: remove => __PROC(remove)
        procedure :: begin => __PROC(begin)
        procedure :: end => __PROC(end)
!!$#ifdef _DUMP_TREE
        procedure :: dump => __PROC(dump)
!!$#endif
        procedure :: deepCopy => __PROC(deepCopy)
        generic :: assignment(=) => deepCopy
        procedure :: equalSets
        generic :: operator(==) => equalSets
        procedure :: notEqualSets
        generic :: operator(/=) => notEqualSets
        procedure, private :: get_child
        procedure, private :: set_child
        procedure, private :: set_parent_child
        procedure, private :: find_index
        procedure, private :: update_height
        procedure, private :: rebalance
        procedure, private :: erase_nonleaf
        procedure, private :: advpos
        procedure, private :: rot
      end type __set

      type :: __iterator
        private
        type (__set), pointer :: reference => null()
        integer(kind=SIZE_KIND) :: current = UNINITIALIZED
      contains
        procedure :: value => __PROC(value)
        procedure :: next => __PROC(next)
        procedure :: prev => __PROC(prev)
        procedure :: equalIters => __PROC(equalIters)
        procedure :: notEqualIters => __PROC(notEqualIters)
        generic :: operator(==) => equalIters
        generic :: operator(/=) => notEqualIters
      end type __iterator

#include "templates/all_template_macros_undefs.inc"

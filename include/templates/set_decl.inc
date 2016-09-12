#include "templates/all_template_macros.inc"

      type :: node
        type(node), pointer :: parent=>null()
        type(node), pointer :: left=>null()
        type(node), pointer :: right=>null()
        integer :: height=1
        __type_declare_component :: value

      contains
        final :: __PROC(kill_node)
        procedure :: child => __PROC(child)
        procedure :: whichchild => __PROC(whichchild)
        procedure :: setchild => __PROC(setchild)
        procedure, private :: updateheight => __PROC(updateheight)
      end type node

      type :: __set
        private
        type(node), pointer :: root=>null()
        integer(kind=SIZE_KIND) :: tsize=0
      contains
        final :: __PROC(kill_tree)
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
#ifdef _DUMP_TREE
        procedure :: dump => __PROC(dump)
#endif
        procedure :: deepCopy => __PROC(deepCopy)
        generic :: assignment(=) => deepCopy
        procedure :: equalSets
        generic :: operator(==) => equalSets
        procedure :: notEqualSets
        generic :: operator(/=) => notEqualSets
        procedure, private :: findnode
        procedure, private :: rebalance
        procedure, private :: erasenonleaf
        procedure, private :: advpos
        procedure, private :: rot
      end type __set

      type :: __iterator
        private
        type(__set), pointer :: tree=>null()
        type(node), pointer :: node=>null()
#ifdef __GFORTRAN__
        ! This workaround prevents the need for an empty
        ! FINAL procedure
        ! Bug should be submitted to GNU
        integer, allocatable :: a(:)
#endif
      contains
        procedure :: good => __PROC(good)
        procedure :: value => __PROC(value)
        procedure :: next => __PROC(next)
        procedure :: prev => __PROC(prev)
        procedure :: equalIters => __PROC(equalIters)
        procedure :: notEqualIters => __PROC(notEqualIters)
        generic :: operator(==) => equalIters
        generic :: operator(/=) => notEqualIters
      end type __iterator

#include "templates/all_template_macros_undefs.inc"

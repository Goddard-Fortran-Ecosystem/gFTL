!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

#ifndef __COMPARE
#  define __COMPARE(x, y) x<y
#endif

      type, private :: node
        type(node), pointer :: parent=>null()
        type(node), pointer :: left=>null()
        type(node), pointer :: right=>null()
        integer :: height=1
#ifdef _wrapentry
        _wrapentry :: value
#else
        _entry :: value
#endif
      contains
        final :: kill_node
        procedure :: child
        procedure :: whichchild
        procedure :: setchild
        procedure, private :: updateheight
      end type node

      type, public :: tree
        private
        type(node), pointer :: root=>null()
        integer :: tsize=0
      contains
        final :: kill_tree
        procedure :: empty
        procedure :: getsize
        procedure :: find
        procedure :: clear
        procedure :: insert
        procedure :: erase
        procedure :: remove
        procedure :: ibegin
        procedure :: iend
#ifdef _DUMP_TREE
        procedure :: dump
#endif
        procedure, private :: findnode
        procedure, private :: rebalance
        procedure, private :: erasenonleaf
        procedure, private :: advpos
        procedure, private :: rot
      end type tree

      type, public :: iter
        private
        type(tree), pointer :: tree=>null()
        type(node), pointer :: node=>null()
      contains
        procedure :: good
        procedure :: value
        procedure :: next
        procedure :: prev
        procedure :: equal
        procedure :: nequal
        generic :: operator(==) => equal
        generic :: operator(/=) => nequal
        final :: kill_iter
      end type iter

#ifdef _pointer
#define _type _entry
#include "pointerdef.inc"
#endif

! =======================
!  kill_node
! =======================
      subroutine kill_node(this)
      implicit none
      type(node), intent(inout) :: this

      if (associated(this%left)) deallocate(this%left)
      if (associated(this%right)) deallocate(this%right)
      return
      end subroutine kill_node

! =======================
!  child
! =======================
      function child(this, idx)
      implicit none
      class(node), intent(in) :: this
      integer :: idx
      type(node), pointer :: child

      if (idx==0) child=>this%left
      if (idx==1) child=>this%right
      return
      end function child

! =======================
!  whichchild
! =======================
      function whichchild(this, child)
      implicit none
      class(node), intent(in) :: this
      type(node), target, intent(in) :: child
      integer :: whichchild

      whichchild=merge(0, 1, associated(this%left, target=child))
      return
      end function whichchild

! =======================
!  setchild
! =======================
      subroutine setchild(this, idx, child)
      implicit none
      class(node), intent(inout) :: this
      integer, intent(in) :: idx
      type(node), pointer, intent(in) :: child

      if (idx==0) this%left=>child
      if (idx==1) this%right=>child
      return
      end subroutine setchild

! =======================
!  updateheight
! =======================
      subroutine updateheight(this)
      implicit none
      class(node), target, intent(inout) :: this
      type(node), pointer :: p
      integer :: h0, h1

      h0=0
      h1=0
      if (associated(this%left)) h0=this%left%height
      if (associated(this%right)) h1=this%right%height
      this%height=max(h0, h1)+1
      return
      end subroutine updateheight

!=========================================================================

! =======================
!  kill_tree
! =======================
      subroutine kill_tree(this)
      implicit none
      type(tree), intent(inout) :: this

      if (associated(this%root)) deallocate(this%root)
      return
      end subroutine kill_tree

! =======================
!  empty
! =======================
      function empty(this)
      implicit none
      class(tree), intent(in) :: this
      logical :: empty

      empty=.not.associated(this%root)
      return
      end function empty

! =======================
!  getsize
! =======================
      function getsize(this)
      implicit none
      class(tree), intent(in) :: this
      integer :: getsize

      getsize=this%tsize
      return
      end function getsize

! =======================
!  find
! =======================
      function find(this, value)
      implicit none
      class(tree), target, intent(in) :: this
      _entry, target, intent(in) :: value
      type(iter) :: find
      integer :: side

      find%tree=>this
      find%node=>this%findnode(value, .false.)
      if (.not.(__EQ(find%node%value, value))) find%node=>null()
      return
      end function find

! =======================
!  clear
! =======================
      subroutine clear(this)
      implicit none
      class(tree), intent(inout) :: this

      if (associated(this%root)) deallocate(this%root)
      this%tsize=0
      return
      end subroutine clear

! =======================
!  insert
! =======================
      subroutine insert(this, value, ref)
      implicit none
      class(tree), intent(inout) :: this
      _entry, target, intent(in) :: value
      type(node), pointer, optional, intent(out) :: ref
      type(node), pointer :: new, parent

      if (associated(this%root)) then
#ifdef _multi
        parent=>this%findnode(value, .true.)
#else
        parent=>this%findnode(value, .false.)
        if (__EQ(parent%value, value)) then
          if (present(ref)) then
            ref=>parent
          else
            __SET(parent%value, value)
          endif
          return
        endif
#endif
        allocate(new)
        if (present(ref)) ref=>new
        new%parent=>parent
        __SET(new%value, value)
        call parent%setchild(merge(0, 1,                                 &
     &    __COMPARE(value, parent%value)), new)
        call this%rebalance(parent, .true.)
      else
        allocate(this%root)
        if (present(ref)) ref=>this%root
        __SET(this%root%value, value)
      endif
      this%tsize=this%tsize+1
      return
      end subroutine insert

! =======================
!  erase
! =======================
      subroutine erase(this, it)
      implicit none
      class(tree), intent(inout) :: this
      type(iter), intent(inout) :: it
      type(node), pointer :: pos, parent

      pos=>it%node
      call it%next
      if (associated(pos%right)) then
        call this%erasenonleaf(pos, 1)
      else if (associated(pos%left)) then
        call this%erasenonleaf(pos, 0)
      else
        parent=>pos%parent
        if (associated(parent)) then
          call parent%setchild(parent%whichchild(pos), null())
          call this%rebalance(parent, .false.)
        else
          this%root=>null()
        endif
        deallocate(pos)
      endif
      this%tsize=this%tsize-1
      return
      end subroutine erase

! =======================
!  remove
! =======================
      subroutine remove(this, value)
      implicit none
      class(tree), intent(inout) :: this
      _entry, target, intent(in) :: value
      type(iter) :: it

      it=this%find(value)
      if (it/=this%iend()) call this%erase(it)
      return
      end subroutine remove

! =======================
!  ibegin
! =======================
      function ibegin(this)
      implicit none
      class(tree), target, intent(in) :: this
      type(iter) :: ibegin

      ibegin%tree=>this
      call ibegin%next()
      return
      end function ibegin

! =======================
!  iend
! =======================
      function iend(this)
      implicit none
      class(tree), target, intent(in) :: this
      type(iter) :: iend

      iend%tree=>this
      return
      end function iend

#ifdef _DUMP_TREE
! =======================
!  dump
! =======================
      recursive subroutine dump(this, pos)
      implicit none
      class(tree), intent(in) :: this
      type(node), pointer, intent(in), optional :: pos

      if (present(pos)) then
        if (associated(pos%left)) call this%dump(pos%left)
        write(*, *)pos%value, loc(pos), loc(pos%parent), loc(pos%left),  &
     &       loc(pos%right), pos%height
        if (associated(pos%right)) call this%dump(pos%right)
      else
        write(*, *)'size=',this%getsize()
        write(*, *)'root=',loc(this%root)
        call this%dump(this%root)
      endif
      return
      end subroutine dump
#endif

! =======================
!  findnode
! =======================
      function findnode(this, value, last)
      implicit none
      class(tree), target, intent(in) :: this
      _entry, target, intent(in) :: value
      logical, intent(in) :: last
      type(node), pointer :: findnode
      type(node), pointer :: child
      integer :: side

      findnode=>this%root
      if (associated(findnode)) then
        do
          if (.not.last.and.(                                            &
     &      (findnode%value== value))) return
          side=merge(0, 1, __COMPARE(value, findnode%value))
          child=>findnode%child(side)
          if (.not.associated(findnode%child(side))) return
          findnode=>findnode%child(side)
        end do
      end if

      return
      end function findnode

! =======================
!  rebalance
! =======================
      subroutine rebalance(this, pos, once)
      implicit none
      class(tree), intent(inout) :: this
      type(node), pointer, intent(in) :: pos
      logical, intent(in) :: once
      type(node), pointer :: curr, child
      integer :: hl, hr, chl, chr, side, childside
      logical :: unbalanced

      curr=>pos
      do while (associated(curr))
        hl=0
        hr=0
        if (associated(curr%left)) hl=curr%left%height
        if (associated(curr%right)) hr=curr%right%height
        unbalanced=abs(hl-hr)>1
        if (unbalanced) then
          side=merge(0, 1, hl>hr)
          child=>curr%child(side)
          chl=0
          chr=0
          if (associated(child%left)) chl=child%left%height
          if (associated(child%right)) chr=child%right%height
          if (chr/=chl) then
            childside=merge(0, 1, chl>chr)
            if (side/=childside) call this%rot(child, 1-childside)
            call this%rot(curr, 1-side)
          endif
        endif
        call curr%updateheight
        if (unbalanced.and.once) return
        curr=>curr%parent
      end do
      return
      end subroutine rebalance

! =======================
!  erasenonleaf
! =======================
      subroutine erasenonleaf(this, pos, side)
      implicit none
      class(tree), intent(inout) :: this
      type(node), pointer, intent(inout) :: pos
      integer, intent(in) :: side
      type(node), pointer :: parent, other, child0, child1,              &
     &                                       otherchild, otherparent

      parent=>pos%parent
      other=>pos
      call this%advpos(other, side)
      child0=>pos%child(side)
      child1=>pos%child(1-side)
      otherchild=>other%child(side)
      otherparent=>other%parent
      other%parent=>parent
      if (associated(parent)) then
        call parent%setchild(parent%whichchild(pos), other)
      else
        this%root=>other
      endif
      call other%setchild(1-side, child1)
      if (associated(child1)) child1%parent=>other
      if (associated(other, target=child0)) then
        call this%rebalance(other, .false.)
      else
        call other%setchild(side, child0)
        child0%parent=>other
        call otherparent%setchild(1-side, otherchild)
        if (associated(otherchild)) otherchild%parent=>otherparent
        call this%rebalance(otherparent, .false.)
      endif
      pos%left=>null()
      pos%right=>null()
      deallocate(pos)
      return
      end subroutine erasenonleaf

! =======================
!  advpos
! =======================
      subroutine advpos(this, pos, dir)
      implicit none
      class(tree), intent(in) :: this
      type(node), pointer, intent(inout) :: pos
      integer, intent(in) :: dir   ! dir=1 forward, dir=0 backward
      type(node), pointer :: prev

      if (.not.associated(pos)) then
        if (.not.associated(this%root)) return
        pos=>this%root
        do while (associated(pos%child(1-dir)))
          pos=>pos%child(1-dir)
        end do
      else if (associated(pos%child(dir))) then
        pos=>pos%child(dir)
        do while (associated(pos%child(1-dir)))
          pos=>pos%child(1-dir)
        end do
      else
        prev=>pos
        pos=>pos%parent
        do while (associated(pos))
          if (.not.associated(pos%child(dir), prev)) exit
          prev=>pos
          pos=>pos%parent
        end do
      endif
      return
      end subroutine advpos

! =======================
!  rot
! =======================
      subroutine rot(this, pos, dir)
      implicit none
      class(tree), intent(inout) :: this
      type(node), pointer, intent(in) :: pos
      integer, intent(in) :: dir
      type(node), pointer :: parent, child, grandchild=>null()

      parent=>pos%parent
      child=>pos%child(1-dir)
      if (associated(child)) grandchild=>child%child(dir)
      if (associated(parent)) then
        call parent%setchild(parent%whichchild(pos), child)
      else
        this%root=>child
      endif
      pos%parent=>child
      call pos%setchild(1-dir, grandchild)
      if (associated(child)) then
        child%parent=>parent
        call child%setchild(dir, pos)
        if (associated(grandchild)) grandchild%parent=>pos
      endif
      call pos%updateheight
      if (associated(child)) call child%updateheight
      return
      end subroutine rot

!=========================================================================

! =======================
!  kill_iter
! =======================
! doesn't do anything, needed because of internal compiler error in gfortran 4.9.1
      subroutine kill_iter(this)
      implicit none
      type(iter), intent(inout) :: this

      return
      end subroutine kill_iter

! =======================
!  good
! =======================
      function good(this)
      implicit none
      class(iter), intent(in) :: this
      logical :: good

      good=associated(this%node)
      return
      end function good

! =======================
!  value
! =======================
      function value(this)
      implicit none
      class(iter), intent(in) :: this
      _retentry, pointer :: value

      value=>this%node%value
      return
      end function value

! =======================
!  next
! =======================
      subroutine next(this)
      implicit none
      class(iter), intent(inout) :: this

      call this%tree%advpos(this%node, 1)
      return
      end subroutine next

! =======================
!  prev
! =======================
      subroutine prev(this)
      implicit none
      class(iter), intent(inout) :: this

      call this%tree%advpos(this%node, 0)
      return
      end subroutine prev

! =======================
!  equal
! =======================
      function equal(this, other)
      implicit none
      class(iter), intent(in) :: this, other
      logical :: equal

      equal=associated(this%tree, target=other%tree).and.
     &   ((.not.associated(this%node).and..not.associated(other%node))   &
     &   .or.associated(this%node, target=other%node))
      return
      end function equal

! =======================
!  nequal
! =======================
      function nequal(this, other)
      implicit none
      class(iter), intent(in) :: this, other
      logical :: nequal

      nequal=.not.equal(this, other)
      return
      end function nequal

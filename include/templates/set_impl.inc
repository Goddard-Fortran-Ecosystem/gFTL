#include "templates/all_template_macros.inc"

#if defined(_pointer)
#  define __pointer_declare_result __type_declare_result
#  define __pointer_declare_dummy  __type_declare_dummy
#  define __need_compare
#  include "pointerdef.inc"
#  undef __pointer_declare_result
#  undef __pointer_declare_dummy
#elif (__type_rank > 0) & !defined(_extents)
#  define __array_declare_dummy __type_declare_dummy
#  define __ARRAY_EQ_ELEMENT(x,y) __TYPE_EQ_ELEMENT(x,y)
#  include "array_defs.inc"
#  undef __ARRAY_EQ_ELEMENT
#  undef __array_declare_dummy
#endif


! =======================
!  kill_node
! =======================
      recursive subroutine __PROC(kill_node)(this)
      type(node), intent(inout) :: this
      
      if (associated(this%left)) deallocate(this%left)
      if (associated(this%right)) deallocate(this%right)
      return
      end subroutine __PROC(kill_node)

! =======================
!  child
! =======================
      function __PROC(child)(this, idx) result(child)
      class(node), target, intent(in) :: this
      integer :: idx
      type(node), pointer :: child

      if (idx==0) child=>this%left
      if (idx==1) child=>this%right

      end function __PROC(child)

! =======================
!  whichchild
! =======================
      function __PROC(whichchild)(this, child) result(whichchild)
      class(node), intent(in) :: this
      type(node), target, intent(in) :: child
      integer :: whichchild

      whichchild=merge(0, 1, associated(this%left, target=child))

      end function __PROC(whichchild)

! =======================
!  setchild
! =======================
      subroutine __PROC(setchild)(this, idx, child)
      class(node), intent(inout) :: this
      integer, intent(in) :: idx
      type(node), pointer, intent(in) :: child

      if (idx==0) this%left=>child
      if (idx==1) this%right=>child
      return
      end subroutine __PROC(setchild)

! =======================
!  updateheight
! =======================
      subroutine __PROC(updateheight)(this)
      class(node), target, intent(inout) :: this
      integer :: h0, h1

      h0=0
      h1=0
      if (associated(this%left)) h0=this%left%height
      if (associated(this%right)) h1=this%right%height
      this%height=max(h0, h1)+1
      return
      end subroutine __PROC(updateheight)

!=========================================================================

! =======================
!  kill_tree
! =======================
      recursive subroutine __PROC(kill_tree)(this)
      type(__set), intent(inout) :: this

      if (associated(this%root)) deallocate(this%root)

      return
      end subroutine __PROC(kill_tree)

! =======================
!  empty
! =======================
      logical function __PROC(empty)(this) result(empty)
      class(__set), intent(in) :: this

      empty = .not. associated(this%root)

      end function __PROC(empty)

! =======================
!  size
! =======================
      function __PROC(size)(this) result(size)
         integer(kind=SIZE_KIND) :: size
         class(__set), intent(in) :: this

         size = this%tsize

      end function __PROC(size)

! =======================
!  find
! =======================
      function __PROC(find)(this, value) result(find)
      class(__set), target, intent(in) :: this
      __type_declare_dummy, intent(in) :: value
      type(__iterator) :: find

      find%tree=>this
      find%node=>this%findnode(value, .false.)

      if (associated(find%node)) then
         if (.not.__PROC(orderEq)(                                              &
     &                    find%node%value,value)) then
            find%node=>null()
         end if
      end if

      return
      end function __PROC(find)


      logical function __PROC(orderEq)(x, y) result(equal)
         __type_declare_dummy, intent(in) :: x
         __type_declare_dummy, intent(in) :: y

         equal = .not. __PROC(lessThan)(x,y) .and.                             &
     &           .not. __PROC(lessThan)(y,x)
      end function __PROC(orderEq)


! =======================
!  count
! =======================
      function __PROC(count)(this, value) result(count)
      integer(kind=SIZE_KIND) :: count
      class(__set), target, intent(in) :: this
      __type_declare_dummy, intent(in) :: value

      type (__iterator) :: i

      i = this%find(value)

      if (associated(i%node)) then
         count = 1
      else 
         count = 0
      end if

      end function __PROC(count)

! =======================
!  clear
! =======================
      subroutine __PROC(clear)(this)
      class(__set), intent(inout) :: this

      if (associated(this%root)) deallocate(this%root)
      this%tsize=0
      return
      end subroutine __PROC(clear)

! =======================
!  insert
! =======================
      subroutine __PROC(insert)(this, value, unused, isNew, ref)
      class(__set), intent(inout) :: this
      __type_declare_dummy, intent(in) :: value
      type (Unusable), optional :: unused
      logical, optional, intent(out) :: isNew
      type(node), pointer, optional, intent(out) :: ref
      type(node), pointer :: new
      type(node), pointer :: parent

      type (node), pointer :: r
#ifdef _pointer
      __type_declare_result, pointer :: p
#endif

      if (present(unused)) print*,shape(unused)
      
      if (associated(this%root)) then
#ifdef _multi
        parent=>this%findnode(value, .true.)
#else
        parent=>this%findnode(value, .false.)
        if (__PROC(orderEq)(parent%value, value)) then
           if (present(ref)) then
              ref=>parent
           else
              __TYPE_FREE(parent%value)
              __TYPE_ASSIGN(parent%value, value)
           endif
           if (present(isNew)) then
              isNew = .false.
           end if
           return
        endif
#endif
        if (present(isNew)) then
           isNew = .true.
        end if

        allocate(new)
        if (present(ref)) ref=>new
        new%parent=>parent
        __TYPE_ASSIGN(new%value, value)
#ifdef _pointer
        p => value
        call parent%setchild(merge(0, 1,                                        &
     &       __PROC(lessThan)(p, parent%value)),new)
#else
        call parent%setchild(merge(0, 1,                                        &
     &       __PROC(lessThan)(value, parent%value)),new)
#endif
        call this%rebalance(parent, .true.)
      else
        allocate(this%root)
        if (present(ref)) ref=>this%root
        r => this%root
        __TYPE_ASSIGN(r%value, value)
        if (present(isNew)) then
           isNew = .true.
        end if
      endif
      this%tsize = this%tsize + 1
      return
      end subroutine __PROC(insert)

      logical function __PROC(lessThan)(x, y) result(less)
         __type_declare_dummy, intent(in) :: x
         __type_declare_dummy, intent(in) :: y

         less = __TYPE_LESS_THAN(x,y)

      contains

! TODO:  possibly this procedure should be inside some sort of #ifdef
         logical function dictionaryLessThan1d(x, y) result(less)
            integer, intent(in) :: x(:)
            integer, intent(in) :: y(:)

            integer(kind=SIZE_KIND) :: i, n
            
            n = min(size(x),size(y))
            
            do i = 1, n
               less = (x(i) < y(i))
               if (.not. x(i) == y(i)) return
            end do
            
            less = (size(x) < size(y))
            
         end function dictionaryLessThan1d

#if defined(__type_needs_default_compare)
#  if !defined(__compare_type)
#    define __compare_declare_dummy __type_declare_dummy
#    define __compare_declare_component __type_declare_component
#    define __COMPARE_ASSIGN(x,y) __TYPE_ASSIGN(x,y)
#    define __COMPARE_FREE(x) __TYPE_FREE(x)
#  endif

         logical function defaultLessThan(x, y) result(less)
            __compare_declare_dummy, intent(in) :: x
            __compare_declare_dummy, intent(in) :: y

#  if  defined(_pointer)
            type LocalWrapper
               __compare_declare_component :: item
            end type LocalWrapper

            type (LocalWrapper) :: wrapX, wrapY
#    define __xx wrapX
#    define __yy wrapY

            __COMPARE_ASSIGN(wrapX%item, x)
            __COMPARE_ASSIGN(wrapY%item, y)

#  else

#    define __xx x
#    define __yy y

#  endif

            associate( wx => transfer(__xx,[1]),                                &
     &                 wy => transfer(__yy,[1]) )
              less = dictionaryLessThan1d(wx, wy)
            end associate

#  if  defined(_pointer)
            __COMPARE_FREE(wrapX%item)
            __COMPARE_FREE(wrapY%item)
#  endif
#  undef __xx
#  undef __yy
#  if !defined(__compare_type)
#    undef __compare_declare_dummy
#    undef __compare_declare_component
#    undef __COMPARE_ASSIGN
#    undef __COMPARE_FREE
#  endif
         end function defaultLessThan
#endif
      
      end function __PROC(lessThan)



         
         

! =======================
!  erase
! =======================
      subroutine __PROC(erase_one)(this, iter)
      class(__set), intent(inout) :: this
      type(__iterator), intent(inout) :: iter

      type (__iterator) :: last

      last = iter
      call last%next()
      call this%erase(iter, last)

      end subroutine __PROC(erase_one)


! =======================
!  erase_multi
! =======================
      subroutine __PROC(erase_multi)(this, first, last)
      class(__set), intent(inout) :: this
      type(__iterator), intent(inout) :: first
      type(__iterator), intent(in) :: last
      type(node), pointer :: pos, parent

      type (__iterator) :: iter

      iter = first
      do while (iter /= last)
         pos=>iter%node
         call iter%next()
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
      end do
      first = last

      return
      end subroutine __PROC(erase_multi)

! =======================
!  remove
! =======================
      subroutine __PROC(remove)(this, value)
      class(__set), target, intent(inout) :: this
      __type_declare_dummy, intent(in) :: value
      type(__iterator) :: it

      it=this%find(value)
      if (it/=this%end()) call this%erase(it)
      return
      end subroutine __PROC(remove)

! =======================
!  begin
! =======================
      function __PROC(begin)(this) result(begin)
      class(__set), target, intent(in) :: this
      type(__iterator) :: begin

      begin%tree=>this
      call begin%next()
      return
      end function __PROC(begin)

! =======================
!  end
! =======================
      function __PROC(end)(this) result(end_)
      class(__set), target, intent(in) :: this
      type(__iterator) :: end_

      end_%tree=>this
      return
      end function __PROC(end)


#ifdef _DUMP_TREE
! =======================
!  dump
! =======================
      recursive subroutine __PROC(dump)(this, pos)
      class(__set), intent(in) :: this
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
      end subroutine __PROC(dump)
#endif

! =======================
!  findnode
! =======================
      function findnode(this, value, last)
      class(__set), target, intent(in) :: this
      __type_declare_dummy, intent(in) :: value
#ifndef _pointer
!!$      target :: value
#endif
      logical, intent(in) :: last
      type(node), pointer :: findnode
      integer :: side

#ifdef _pointer
      __type_declare_result, pointer :: p
#endif

      findnode=>this%root
      if (associated(findnode)) then
        do
          if (.not. last .and. (                                           &
     &      (__PROC(orderEq)(findnode%value,value)))) return
#ifdef _pointer
          p => value
          side=merge(0, 1, __PROC(lessThan)(p, findnode%value))
#else
          side=merge(0, 1, __PROC(lessThan)(value, findnode%value))
#endif
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
      class(__set), intent(inout) :: this
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
      class(__set), intent(inout) :: this
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
      class(__set), target, intent(in) :: this
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
      class(__set), intent(inout) :: this
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
!  good
! =======================
      function __PROC(good)(this) result(good)
      class(__iterator), intent(in) :: this
      logical :: good

      good=associated(this%node)

      end function __PROC(good)

! =======================
!  value
! =======================
      function __PROC(value)(this) result(value)
      class(__iterator), intent(in) :: this
      __type_declare_result, pointer :: value

      value=>this%node%value

      end function __PROC(value)

! =======================
!  next
! =======================
      subroutine __PROC(next)(this)
      class(__iterator), intent(inout) :: this

      call this%tree%advpos(this%node, 1)
 
      end subroutine __PROC(next)

! =======================
!  prev
! =======================
      subroutine __PROC(prev)(this)
      class(__iterator), intent(inout) :: this

      call this%tree%advpos(this%node, 0)
 
      end subroutine __PROC(prev)

! =======================
!  equalIters
! =======================
      logical function __PROC(equalIters)(this, other)
      class(__iterator), intent(in) :: this, other

      __PROC(equalIters) =                                                       &
     &    associated(this%tree, target=other%tree) .and.                         &
     &   ((.not.associated(this%node) .and. .not.associated(other%node))         &
     &   .or.associated(this%node, target=other%node))

      end function __PROC(equalIters)

! =======================
!  nequal
! =======================
      logical function __PROC(notEqualIters)(this, other)
      implicit none
      class(__iterator), intent(in) :: this, other

      __PROC(notEqualIters) = .not. (this == other)
      
      end function __PROC(notEqualIters)


! =======================
!  equalSets
! =======================
      logical function equalSets(this, other)
      class(__set), target, intent(in) :: this
      class(__set), target, intent(in) :: other

      type (__iterator) :: iter
      __type_declare_result, pointer :: ptr

      equalSets = .false. ! unless
#if  !defined(__INTEL_COMPILER) | !(defined(_string) & !defined(_string_deferred))
      if (this%size() /= other%size()) return

      iter = this%begin()
      do while (iter /= this%end())
         ptr => iter%value()
         if (other%count(ptr) == 0) then
            return
         end if
         call iter%next()
      end do
      
      equalSets = .true.
#endif
      end function equalSets


! =======================
!  notEqualSets
! =======================
      logical function notEqualSets(this, other)
      class(__set), intent(in) :: this, other

      notEqualSets = .not. (this == other)

      end function notEqualSets


! =======================
!   deepCopy (assignment)
! =======================
      subroutine __PROC(deepCopy)(this, other)
         class (__set), target, intent(out) :: this
         class (__set), target, intent(in) :: other

         type (__iterator) :: iter
         __type_declare_result, pointer :: ptr

#if  !defined(__INTEL_COMPILER) | !(defined(_string) & !defined(_string_deferred))
         iter = other%begin()
         do while (iter /= other%end())
            ptr => iter%value()
            call this%insert(ptr)
            call iter%next()
         end do
#endif
         this%tsize = other%tsize

      end subroutine __PROC(deepCopy)

#include "templates/all_template_macros_undefs.inc"


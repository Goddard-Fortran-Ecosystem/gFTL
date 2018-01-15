!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

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
!  exists - a helper function
! =======================
      logical function exists(pos)
         integer(kind=SIZE_KIND), intent(in) :: pos
         exists = (pos /= UNINITIALIZED)
      end function exists


! =======================
!  opposite - a helper function
! =======================
      integer function opposite(dir)
         integer, intent(in) :: dir
         
         opposite = (1 - dir)
         
      end function opposite


! =======================
!  child
! =======================
      function get_child(this, pos, dir) result(child)
         integer(kind=SIZE_KIND) :: child
         class(__set), intent(in) :: this
         integer(kind=SIZE_KIND), intent(in) :: pos
         integer, intent(in) :: dir

         select case (dir)
         case (LEFT)
            child = this%lefts%at(pos)
         case (RIGHT)
            child = this%rights%at(pos)
         end select

      end function get_child

! =======================
!  set_child
! =======================
      subroutine set_child(this, pos, dir, child)
         class(__set), intent(inout) :: this
         integer(kind=SIZE_KIND), intent(in) :: pos
         integer, intent(in) :: dir
         integer(kind=SIZE_KIND), intent(in) :: child

         select case (dir)
         case (LEFT)
           if (child == 0) then
           end if
            call this%lefts%set(pos, child)
         case (RIGHT)
            call this%rights%set(pos, child)
         end select

      end subroutine set_child

! =======================
!  set_parent_child
! =======================
      subroutine set_parent_child(this, parent, pos, other)
         class(__set), intent(inout) :: this
         integer(kind=SIZE_KIND), intent(in) :: parent
         integer(kind=SIZE_KIND), intent(in) :: pos
         integer(kind=SIZE_KIND), intent(in) :: other

         if (this%lefts%at(parent) == pos) then
            call this%set_child(parent, LEFT, other)
         else
            call this%set_child(parent, RIGHT, other)
         end if

      end subroutine set_parent_child

! =======================
!  update_height
! =======================
      subroutine update_height(this, pos) 
         class (__set), intent(inout) :: this
         integer(kind=SIZE_KIND), intent(in) :: pos

         integer(kind=SIZE_KIND) :: idx
         integer(kind=SIZE_KIND) :: new_height

         new_height = 0

         idx = this%lefts%at(pos)
         if (exists(idx)) then
            new_height = this%heights%at(idx)
         end if

         idx = this%rights%at(pos)
         if (exists(idx)) then
            new_height = max(new_height, this%heights%at(idx))
         end if
         
         new_height = new_height + 1
         call this%heights%set(pos, new_height)

      end subroutine update_height

!=========================================================================

! =======================
!  empty
! =======================
      logical function __PROC(empty)(this) result(empty)
      class(__set), intent(in) :: this

      empty = (.not. exists(this%root))

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

#ifdef _pointer
      __type_declare_result, pointer :: q
#endif
      
      find%reference => this
      associate (c => find%current)
        c = this%find_index(value, .false.)

        if (exists(c)) then
#ifdef _pointer
           q => this%items%at(c)
           if (.not.__PROC(orderEq)(                                                     &
     &                    q,value)) then
              c = UNINITIALIZED
           end if
#else
           if (.not.__PROC(orderEq)(                                                     &
     &                    this%items%at(c),value)) then
              c = UNINITIALIZED
           end if
#endif


        end if

      end associate

      end function __PROC(find)


      logical function __PROC(orderEq)(x, y) result(equal)
         __type_declare_dummy, intent(in) :: x
         __type_declare_dummy, intent(in) :: y

         equal = .not. __PROC(lessThan)(x,y) .and.                            &
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

      if (.not. exists(i%current)) then
         count = 0
      else 
         count = 1
      end if

      end function __PROC(count)

! =======================
!  clear
! =======================
      subroutine __PROC(clear)(this)
      class(__set), intent(inout) :: this

      call this%items%clear()
      call this%parents%clear()
      call this%lefts%clear()
      call this%rights%clear()
      call this%heights%clear()

      this%root = UNINITIALIZED
      this%tsize=0

      end subroutine __PROC(clear)

! =======================
!  insert
! =======================
      subroutine __PROC(insert)(this, value, unused, isNew, ref)
      class(__set), intent(inout) :: this
      __type_declare_dummy, intent(in) :: value
      type (Unusable), optional :: unused
      logical, optional, intent(out) :: isNew

      integer(kind=SIZE_KIND), optional, intent(out) :: ref

      integer(kind=SIZE_KIND) :: new
      integer(kind=SIZE_KIND) :: parent
      logical :: eq
#ifdef _pointer
      __type_declare_result, pointer :: p, q
#endif

      if (present(unused)) print*,shape(unused)
      
      if (exists(this%root)) then
#ifdef _multi
        parent = this%find_index(value, .true.)
#else
        parent = this%find_index(value, .false.)


#ifdef _pointer
        p => this%items%at(parent)
        q => value
        eq = __PROC(orderEq)(p, q)
#else
        if (exists(parent)) then
           eq = __PROC(orderEq)(this%items%at(parent), value)
        else
           eq = .false.
        end if
#endif
        if (eq) then
           if (present(ref)) then
              ref = parent
           else
              call this%items%set(parent, value)
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

        if (this%next_free == 0) then
           call this%items%push_back(value)
           new = this%items%size()
           call this%heights%push_back(1_SIZE_KIND)
           call this%lefts%push_back(UNINITIALIZED)
           call this%rights%push_back(UNINITIALIZED)
           call this%parents%push_back(parent)
        else
           new = this%next_free
           this%next_free = this%parents%at(new)
           call this%items%set(new, value)
           call this%heights%set(new, 1_SIZE_KIND)
           call this%lefts%set(new, UNINITIALIZED)
           call this%rights%set(new, UNINITIALIZED)
           call this%parents%set(new, parent)
        end if

        if (present(ref)) ref=new

#ifdef _pointer
           p => value
           q => this%items%at(parent)
           if (__PROC(lessThan)(p,q)) then
              call this%lefts%set(parent, new)
           else
              call this%rights%set(parent,new)
           end if
#else
           if (__PROC(lessThan)(value, this%items%at(parent))) then
              call this%lefts%set(parent, new)
           else
              call this%rights%set(parent,new)
           end if
#endif
        call this%rebalance(parent, .true.)

      else ! new root
         
        if (this%next_free == 0) then
           call this%items%push_back(value)
           new = this%items%size()
           call this%heights%push_back(1_SIZE_KIND)
           call this%lefts%push_back(UNINITIALIZED)
           call this%rights%push_back(UNINITIALIZED)
           call this%parents%push_back(UNINITIALIZED)
        else
           new = this%next_free
           this%next_free = this%parents%at(new)
           call this%items%set(new, value)
           call this%heights%set(new, 1_SIZE_KIND)
           call this%lefts%set(new, UNINITIALIZED)
           call this%rights%set(new, UNINITIALIZED)
           call this%parents%set(new, UNINITIALIZED)
        end if
        this%root = new

        if (present(ref)) ref = this%root
        if (present(isNew)) then
           isNew = .true.
        end if

      endif

      this%tsize = this%tsize + 1

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

      integer(kind=SIZE_KIND) :: parent
      integer(kind=SIZE_KIND) :: pos

      type (__iterator) :: iter

      iter = first

      do while (iter /= last)

         pos = iter%current
         call iter%next()

         if (exists(this%rights%at(pos))) then
            call this%erase_nonleaf(pos, 1)
         else if (exists(this%lefts%at(pos))) then
            call this%erase_nonleaf(pos, 0)
         else
            parent = this%parents%at(pos)
            if (exists(parent)) then
               call this%set_parent_child(parent, pos, UNINITIALIZED)
               call this%rebalance(parent, .false.)
            else
               this%root = UNINITIALIZED
            endif
            
            ! declare this space available
            call this%parents%set(pos, this%next_free)
            this%next_free = pos

         endif
         this%tsize = this%tsize - 1
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

      end subroutine __PROC(remove)

! =======================
!  begin
! =======================
      function __PROC(begin)(this) result(begin)
      class(__set), target, intent(in) :: this
      type(__iterator) :: begin

      begin%reference=>this
      call begin%next()

      end function __PROC(begin)

! =======================
!  end
! =======================
      function __PROC(end)(this) result(end_)
      class(__set), target, intent(in) :: this
      type(__iterator) :: end_

      end_%reference=>this

      end function __PROC(end)


! =======================
!  dump
! =======================
      recursive subroutine __PROC(dump)(this)
      class(__set), intent(in) :: this

      integer(kind=SIZE_KIND) :: i

#ifdef _DEBUG
      write(*,'(2x,6(1x,a3,2x))') ' # ','val','par','lft','rht',' at '
#else
      write(*,'(2x,5(1x,a3,2x))') ' # ', 'par', 'lft', 'rht', ' at '
#endif

      do i = 1, this%items%size()
#ifdef _DEBUG
         write(*,'(6(i5,1x))') i,                                              &
     &             this%items%at(i),                                           &
     &             this%parents%at(i),                                         &
     &             this%lefts%at(i), this%rights%at(i),                        &
     &             this%heights%at(i)
#else
         write(*,'(5(i5,1x))') i,                                              &
     &             this%parents%at(i),                                         &
     &             this%lefts%at(i), this%rights%at(i),                        &
     &             this%heights%at(i)
#endif
      end do

      end subroutine __PROC(dump)

! =======================
!  find_index
! =======================
      function find_index(this, value, last) result(idx)
      integer(kind=SIZE_KIND) :: idx
      class(__set), target, intent(in) :: this
      __type_declare_dummy, intent(in) :: value
      logical, intent(in) :: last

      integer (kind=SIZE_KIND) :: child

#ifdef _pointer
      __type_declare_result, pointer :: p, q
#endif

      idx = this%root
      if (exists(idx)) then
         do
#ifdef _pointer
            q => this%items%at(idx)
            if (.not. last .and. (                                           &
     &         __PROC(orderEq)(q,value))) return
#else
            if (.not. last .and. (                                           &
     &         __PROC(orderEq)(this%items%at(idx),value))) return
#endif
#ifdef _pointer
            p => value
            q => this%items%at(idx)
            child=merge(this%lefts%at(idx), this%rights%at(idx),              &
     &         __PROC(lessThan)(p, q))
#else
            child=merge(this%lefts%at(idx), this%rights%at(idx),              &
     &         __PROC(lessThan)(value, this%items%at(idx)))
#endif
          
            if (.not. exists(child)) return
            idx = child
         end do
      end if

      end function find_index

! =======================
!  rebalance
! =======================
      subroutine rebalance(this, pos, once)
      class(__set), intent(inout) :: this
      integer(kind=SIZE_KIND), intent(in) :: pos
      logical, intent(in) :: once

      integer(kind=SIZE_KIND) :: curr, child

      integer :: hl, hr, chl, chr, side, childside
      logical :: unbalanced

      integer(kind=SIZE_KIND), pointer :: pLeft, pRight

      curr = pos

      do while (exists(curr))
         hl = 0
         hr = 0
         pLeft => this%lefts%at(curr)
         if (exists(pLeft)) hl = this%heights%at(pLeft)
         pRight => this%rights%at(curr)
         if (exists(pRight)) hr = this%heights%at(pRight)

         unbalanced = (abs(hl-hr) > 1)

         if (unbalanced) then
            side = merge(LEFT, RIGHT, hl>hr)
            child = this%get_child(curr, side)

            chl=0
            chr=0

            pLeft => this%lefts%at(child)
            if (exists(pLeft)) chl = this%heights%at(pLeft)
            pRight => this%rights%at(child)
            if (exists(pRight)) chr = this%heights%at(pRight)

            if (chr /= chl) then
               childside=merge(0, 1, chl>chr)
               if (side/=childside)                                                &
     &            call this%rot(child,opposite(childside))
               call this%rot(curr, opposite(side))
            endif
         endif
         call this%update_height(curr)


         if (unbalanced.and.once) exit

         curr = this%parents%at(curr)

      end do

      end subroutine rebalance

! =======================
!  erase_nonleaf
! =======================
      subroutine erase_nonleaf(this, pos, side)
      class(__set), intent(inout) :: this
      integer(kind=SIZE_KIND), intent(inout) :: pos
      integer, intent(in) :: side

      integer(kind=SIZE_KIND) :: parent, other, child0, child1,           &
     &      otherchild, otherparent, tmp

      parent = this%parents%at(pos)
      other = pos
      call this%advpos(other, side)

      if (side == 0) then
         child0 = this%lefts%at(pos)
         child1 = this%rights%at(pos)
         otherchild = this%lefts%at(other)
      else
         child0 = this%rights%at(pos)
         child1 = this%lefts%at(pos)
         otherchild = this%rights%at(other)
      end if
      otherparent = this%parents%at(other)
      call this%parents%set(other, parent)

      if (exists(parent)) then
        call this%set_parent_child(parent, pos, other)
      else
        this%root = other
      endif
      call this%set_child(other, 1-side, child1)
      if (exists(child1)) call this%parents%set(child1, other)
      if (other == child0) then
        call this%rebalance(other, .false.)
      else
        call this%set_child(other, side, child0)
        call this%parents%set(child0, other)
        call this%set_child(otherparent, 1-side, otherchild)
        if (exists(otherchild)) then
           call this%parents%set(otherchild, otherparent)
        end if
        call this%rebalance(otherparent, .false.)
      endif

      tmp = this%items%size()

      ! declare this space available
      call this%parents%set(pos, this%next_free)
      this%next_free = pos

      pos = UNINITIALIZED

      end subroutine erase_nonleaf

! =======================
!  advpos
! =======================
      subroutine advpos(this, pos, dir)
      class(__set), target, intent(in) :: this
      integer(kind=SIZE_KIND), intent(inout) :: pos
      integer, intent(in) :: dir   ! dir=1 forward, dir=0 backward

      integer(kind=SIZE_KIND) :: prev
      integer(kind=SIZE_KIND) :: child

      if (.not. exists(pos)) then
         if (.not. exists(this%root)) then
            return
         else
            pos = this%root
            do 
               child = this%get_child(pos, 1-dir)
               if (exists(child)) then
                  pos = child
               else
                  exit
               end if
            end do
         end if
      else
         child = this%get_child(pos, dir)
         if (exists(child)) then
            pos = child
            do 
               child = this%get_child(pos, opposite(dir))
               if (exists(child)) then
                  pos = child
               else
                  exit
               end if
            end do
         else
            prev = pos
            pos = this%parents%at(pos)
            
            do while (exists(pos))
               child = this%get_child(pos, dir)
               if (child /= prev) exit
               prev = pos
               pos = this%parents%at(pos)
            end do
         endif
      end if

      end subroutine advpos

! =======================
!  rot - swap pos with one of its children
! =======================
      subroutine rot(this, pos, dir)
      class(__set), intent(inout) :: this
      integer(kind=SIZE_KIND), intent(in) :: pos
      integer, intent(in) :: dir
      
      integer(kind=SIZE_KIND) :: parent, child, grandchild

      parent = this%parents%at(pos)
      child = this%get_child(pos, opposite(dir))

      if (exists(child)) then
         grandchild = this%get_child(child, dir)
      else
         grandchild = UNINITIALIZED
      end if

      if (exists(parent)) then
         call this%set_parent_child(parent, pos, child)
      else ! pos must be root; make the child root instead
         this%root = child
      endif

      ! 'child' is now my parent
      call this%parents%set(pos, child)

      ! 'grandchild' becomes now my child
      call this%set_child(pos, opposite(dir), grandchild)

      ! fix up child
      if (exists(child)) then
         call this%parents%set(child, parent)
         call this%set_child(child, dir, pos)
         ! and fix up grandchild
         if (exists(grandchild)) then
            call this%parents%set(grandchild, pos)
         end if
      end if

      call this%update_height(pos)
      
      if (exists(child)) call this%update_height(child)

      end subroutine rot

!=========================================================================


! =======================
!  value
! =======================
      function __PROC(value)(this) result(value)
      class(__iterator), target, intent(in) :: this
      __type_declare_result, pointer :: value

      value=>this%reference%items%at(this%current)

      end function __PROC(value)

! =======================
!  next
! =======================
      subroutine __PROC(next)(this)
      class(__iterator), intent(inout) :: this

      call this%reference%advpos(this%current, 1)
 
      end subroutine __PROC(next)

! =======================
!  prev
! =======================
      subroutine __PROC(prev)(this)
      class(__iterator), intent(inout) :: this

      call this%reference%advpos(this%current,0)
 
      end subroutine __PROC(prev)

! =======================
!  equalIters
! =======================
      logical function __PROC(equalIters)(this, other)
      class(__iterator), intent(in) :: this, other

      __PROC(equalIters) = this%current == other%current

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

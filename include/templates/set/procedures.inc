!--------------------------------------------------------------------
! Copyright © 2020 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

#define __UNINITIALIZED -1
#define __LEFT 0
#define __RIGHT 1

#include "templates/all_template_macros.inc"


   ! Constructors
   function new_set_empty() result(s)
      type(__set) :: s
      allocate(s%elements(0))
      s%vsize = 0
   end function new_set_empty


   function __MANGLE(new_set_copy)(x) result(s)
      type(__set) :: s
      type(__set) :: x

      s = x
   end function __MANGLE(new_set_copy)

#ifdef __T_listable__
   function __MANGLE(new_set_initializer_list)(il) result(s)
      type (__set) :: s
      __T_declare_dummy__, dimension(:), intent(in) :: il ! initializer list

      integer :: i

      do i = 1, size(il)
         call s%insert(il(i))
      end do

      return
   end function __MANGLE(new_set_initializer_list)
#endif


   ! Type-bound procedures

   ! =======================
   !  exists - a helper function
   ! =======================
   logical function __MANGLE(exists)(pos)
      integer(kind=__gftl_size_kind), intent(in) :: pos
      exists = (pos /= UNINITIALIZED)
   end function __MANGLE(exists)


   ! =======================
   !  opposite - a helper function
   ! =======================
   integer function __MANGLE(opposite)(dir)
      integer, intent(in) :: dir

      opposite = (1 - dir)

   end function __MANGLE(opposite)


   ! =======================
   !  child
   ! =======================
   function __MANGLE(get_child)(this, pos, dir) result(child)
      integer(kind=__gftl_size_kind) :: child
      class(__set), intent(in) :: this
      integer(kind=__gftl_size_kind), intent(in) :: pos
      integer, intent(in) :: dir

      select case (dir)
      case (__LEFT)
         child = this%lefts%at(pos)
      case (__RIGHT)
         child = this%rights%at(pos)
      end select

   end function __MANGLE(get_child

   ! =======================
   !  set_child
   ! =======================
   subroutine __MANGLE(set_child)(this, pos, dir, child)
      class(__set), intent(inout) :: this
      integer(kind=__gftl_size_kind), intent(in) :: pos
      integer, intent(in) :: dir
      integer(kind=__gftl_size_kind), intent(in) :: child

      select case (dir)
      case (__LEFT)
         if (child == 0) then
         end if
         call this%lefts%set(pos, child)
      case (__RIGHT)
         call this%rights%set(pos, child)
      end select

   end subroutine __MANGLE(set_child

   ! =======================
   !  set_parent_child
   ! =======================
   subroutine __MANGLE(set_parent_child)(this, parent, pos, other)
      class(__set), intent(inout) :: this
      integer(kind=__gftl_size_kind), intent(in) :: parent
      integer(kind=__gftl_size_kind), intent(in) :: pos
      integer(kind=__gftl_size_kind), intent(in) :: other

      if (this%lefts%at(parent) == pos) then
         call this%set_child(parent, __LEFT, other)
      else
         call this%set_child(parent, __RIGHT, other)
      end if

   end subroutine __MANGLE(set_parent_child

   ! =======================
   !  update_height
   ! =======================
   subroutine __MANGLE(update_height)(this, pos) 
      class (__set), intent(inout) :: this
      integer(kind=__gftl_size_kind), intent(in) :: pos

      integer(kind=__gftl_size_kind) :: idx
      integer(kind=__gftl_size_kind) :: new_height

      new_height = 0

      idx = this%lefts%at(pos)
      if (this%exists(idx)) then
         new_height = this%heights%at(idx)
      end if

      idx = this%rights%at(pos)
      if (this%exists(idx)) then
         new_height = max(new_height, this%heights%at(idx))
      end if

      new_height = new_height + 1
      call this%heights%set(pos, new_height)

   end subroutine __MANGLE(update_height)

   !=========================================================================

   ! =======================
   !  empty
   ! =======================
   logical function __MANGLE(empty)(this) result(empty)
      class(__set), intent(in) :: this

      empty = (.not. this%exists(this%root))

   end function __MANGLE(empty)

   ! =======================
   !  size
   ! =======================
   function __MANGLE(size)(this) result(size)
      integer(kind=__gftl_size_kind) :: size
      class(__set), intent(in) :: this

      size = this%tsize

   end function __MANGLE(size)

   ! =======================
   !  max_size()
   ! =======================
   pure function __MANGLE(max_size)() result(res)
      integer(kind=__gftl_size_kind) :: res

      integer(kind=__gftl_size_kind) :: index

      res = huge(index)

      return
   end function __MANGLE(max_size)

   ! =======================
   !  find
   ! =======================
   function __MANGLE(find)(this, value) result(find)
      class(__set), target, intent(in) :: this
      __T_declare_dummy__, intent(in) :: value
      type(__iterator) :: find

      find%reference => this
      associate (c => find%current)
        c = this%find_index(value, .false.)

        if (this%exists(c)) then
           if (.not. __MANGLE(eq)(this%items%at(c),value)) then
              c = __UNINITIALIZED
           end if
        end if

      end associate

   end function __MANGLE(find)


   logical function __MANGLE(order_eq)(x, y) result(equal)
      __T_declare_dummy__, intent(in) :: x
      __T_declare_dummy__, intent(in) :: y

      equal = .not. __MANGLE(lessThan)(x,y) .and.                            &
           &           .not. __MANGLE(lessThan)(y,x)
   end function __MANGLE(order_eq)


   ! =======================
   !  count
   ! =======================
   function __MANGLE(count)(this, value) result(count)
      integer(kind=__gftl_size_kind) :: count
      class(__set), target, intent(in) :: this
      __T_declare_dummy__, intent(in) :: value

      type (__iterator) :: i

      i = this%find(value)

      if (.not. this%exists(i%current)) then
         count = 0
      else 
         count = 1
      end if

   end function __MANGLE(count)

   ! =======================
   !  clear
   ! =======================
   subroutine __MANGLE(clear)(this)
      class(__set), intent(inout) :: this

      call this%items%clear()
      call this%parents%clear()
      call this%lefts%clear()
      call this%rights%clear()
      call this%heights%clear()

      this%root = __UNINITIALIZED
      this%tsize=0

   end subroutine __MANGLE(clear)

   ! =======================
   !  insert
   ! =======================
   subroutine __MANGLE(insert)(this, value, unused, isNew, ref)
      class(__set), intent(inout) :: this
      __T_declare_dummy__, intent(in) :: value
      type (Unusable), optional :: unused
      logical, optional, intent(out) :: isNew

      integer(kind=__gftl_size_kind), optional, intent(out) :: ref

      integer(kind=__gftl_size_kind) :: new
      integer(kind=__gftl_size_kind) :: parent
      logical :: eq

#if (defined(__vector_debug) && defined(_DEBUG___))
      print*,__FILE__,__LINE__
#endif
      if (present(unused)) print*,shape(unused)

      if (this%exists(this%root)) then
#ifdef _multi
         parent = this%find_index(value, .true.)
#else
         parent = this%find_index(value, .false.)


         if (this%exists(parent)) then
            eq = __MANGLE(order_eq)(this%items%at(parent), value)
         else
            eq = .false.
         end if

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

#if (defined(__vector_debug) && defined(_DEBUG___))
         print*,__FILE__,__LINE__, this%next_free
#endif
         if (this%next_free == 0) then
#if (defined(__vector_debug) && defined(_DEBUG___))
            print*,__FILE__,__LINE__
#endif
            call this%items%push_back(value)
            new = this%items%size()
            call this%heights%push_back(1___gftl_size_kind)
            call this%lefts%push_back(__UNINITIALIZED)
            call this%rights%push_back(__UNINITIALIZED)
            call this%parents%push_back(parent)
         else
            new = this%next_free
            this%next_free = this%parents%at(new)
            call this%items%set(new, value)
            call this%heights%set(new, 1___gftl_size_kind)
            call this%lefts%set(new, __UNINITIALIZED)
            call this%rights%set(new, __UNINITIALIZED)
            call this%parents%set(new, parent)
         end if

         if (present(ref)) ref=new

#if (defined(__vector_debug) && defined(_DEBUG___))
         print*,__FILE__,__LINE__
#endif
         if (__MANGLE(lessThan)(value, this%items%at(parent))) then
            call this%lefts%set(parent, new)
         else
            call this%rights%set(parent,new)
         end if

         call this%rebalance(parent, .true.)

      else ! new root

#if (defined(__vector_debug) && defined(_DEBUG___))
         print*,__FILE__,__LINE__, this%next_free
#endif
         if (this%next_free == 0) then
            call this%items%push_back(value)
            new = this%items%size()
            call this%heights%push_back(1___gftl_size_kind)
            call this%lefts%push_back(__UNINITIALIZED)
            call this%rights%push_back(__UNINITIALIZED)
            call this%parents%push_back(__UNINITIALIZED)
         else
            new = this%next_free
            this%next_free = this%parents%at(new)
            call this%items%set(new, value)
            call this%heights%set(new, 1___gftl_size_kind)
            call this%lefts%set(new, __UNINITIALIZED)
            call this%rights%set(new, __UNINITIALIZED)
            call this%parents%set(new, __UNINITIALIZED)
         end if
         this%root = new

         if (present(ref)) ref = this%root
         if (present(isNew)) then
            isNew = .true.
         end if

      endif

      this%tsize = this%tsize + 1

   end subroutine __MANGLE(insert)

   logical function __MANGLE(lessThan)(x, y) result(less)
      __T_declare_dummy__, intent(in) :: x
      __T_declare_dummy__, intent(in) :: y

      less = __T_LT__(x,y)

   contains

      ! TODO:  possibly this procedure should be inside some sort of #ifdef
      logical function dictionaryLessThan1d(x, y) result(less)
         integer, intent(in) :: x(:)
         integer, intent(in) :: y(:)

         integer(kind=__gftl_size_kind) :: i, n

         n = min(size(x),size(y))

         do i = 1, n
            less = (x(i) < y(i))
            if (.not. x(i) == y(i)) return
         end do

         less = (size(x) < size(y))

      end function dictionaryLessThan1d

#if defined(__type_needs_default_compare)
#  if !defined(__compare_type)
#    define __compare_declare_dummy __T_declare_dummy__
#    define __compare_declare_component __T_declare_component__
#    define __COMPARE_COPY__(x,y) __T_COPY__(x,y)
#    define __COMPARE_FREE__(x) __T_FREE__(x)
#  endif

      logical function defaultLessThan(x, y) result(less)
         __compare_declare_dummy, intent(in) :: x
         __compare_declare_dummy, intent(in) :: y

#    define __xx x
#    define __yy y

         associate( wx => transfer(__xx,[1]),                                &
              &                 wy => transfer(__yy,[1]) )
           less = dictionaryLessThan1d(wx, wy)
         end associate

#  undef __xx
#  undef __yy
#  if !defined(__compare_type)
#    undef __compare_declare_dummy
#    undef __compare_declare_component
#    undef __COMPARE_COPY__
#    undef __COMPARE_FREE__
#  endif
      end function defaultLessThan
#endif

   end function __MANGLE(lessThan)






   ! =======================
   !  erase
   ! =======================
   subroutine __MANGLE(erase_one)(this, iter)
      class(__set), intent(inout) :: this
      type(__iterator), intent(inout) :: iter

      type (__iterator) :: last

      last = iter
      call last%next()
      call this%erase(iter, last)

   end subroutine __MANGLE(erase_one)


   ! =======================
   !  erase_multi
   ! =======================
   subroutine __MANGLE(erase_multi)(this, first, last)
      class(__set), intent(inout) :: this
      type(__iterator), intent(inout) :: first
      type(__iterator), intent(in) :: last

      integer(kind=__gftl_size_kind) :: parent
      integer(kind=__gftl_size_kind) :: pos

      type (__iterator) :: iter

      iter = first

      do while (iter /= last)

         pos = iter%current
         call iter%next()

         if (this%exists(this%rights%at(pos))) then
            call this%erase_nonleaf(pos, 1)
         else if (this%exists(this%lefts%at(pos))) then
            call this%erase_nonleaf(pos, 0)
         else
            parent = this%parents%at(pos)
            if (this%exists(parent)) then
               call this%set_parent_child(parent, pos, __UNINITIALIZED)
               call this%rebalance(parent, .false.)
            else
               this%root = __UNINITIALIZED
            endif

            ! declare this space available
            call this%parents%set(pos, this%next_free)
            this%next_free = pos

         endif
         this%tsize = this%tsize - 1
      end do
      first = last

      return
   end subroutine __MANGLE(erase_multi)

   ! =======================
   !  remove
   ! =======================
   subroutine __MANGLE(remove)(this, value)
      class(__set), target, intent(inout) :: this
      __T_declare_dummy__, intent(in) :: value
      type(__iterator) :: it

      it=this%find(value)
      if (it/=this%end()) call this%erase(it)

   end subroutine __MANGLE(remove)

   ! =======================
   !  begin
   ! =======================
   function __MANGLE(begin)(this) result(begin)
      class(__set), target, intent(in) :: this
      type(__iterator) :: begin

      begin%reference=>this
      call begin%next()

   end function __MANGLE(begin)

   ! =======================
   !  end()
   ! =======================
   ! Note - some early versions of gfortran did not like function
   ! results to be called "end"
   function __MANGLE(end)(this) result(end_)
      class(__set), target, intent(in) :: this
      type(__iterator) :: end_

      end_%reference=>this

   end function __MANGLE(end)


   ! =======================
   !  lower_bound()
   ! =======================
   ! NOTE: This implementation is O(N).   A Log(N) implementation should be produced.
   function __MANGLE(lower_bound)(this, val) result(lower_bound)
      type(__iterator) :: lower_bound
      class(__set), target, intent(in) :: this
      __T_declare_dummy__ :: val

      lower_bound%reference => this
      lower_bound%current = this%find_index(val, .false.)

   end function __MANGLE(lower_bound)

   ! =======================
   !  upper_bound()
   ! =======================
   ! NOTE: This implementation is O(N).   A Log(N) implementation should be produced.
   function __MANGLE(upper_bound)(this, val) result(upper_bound)
      type(__iterator) :: upper_bound
      class(__set), target, intent(in) :: this
      __T_declare_dummy__ :: val

      upper_bound = this%end()
      call upper_bound%previous()
      do while (.not. __T_LT__(this%get(), val))
         call upper_bound%previous()
      end do

   end function __MANGLE(upper_bound)


   ! =======================
   !  dump
   ! =======================
   recursive subroutine __MANGLE(dump)(this)
      class(__set), intent(in) :: this

      integer(kind=__gftl_size_kind) :: i

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

   end subroutine __MANGLE(dump)

   ! =======================
   !  find_index
   ! =======================
   function __MANGLE(find_index)(this, value, last) result(idx)
      integer(kind=__gftl_size_kind) :: idx
      class(__set), target, intent(in) :: this
      __T_declare_dummy__, intent(in) :: value
      logical, intent(in) :: last

      integer (kind=__gftl_size_kind) :: child

      idx = this%root
      if (this%exists(idx)) then
         do
            if (.not. last .and. (__MANGLE(order_eq)(this%items%at(idx),value))) return

            child = merge(this%lefts%at(idx), this%rights%at(idx), &
                 &         __MANGLE(lessThan)(value, this%items%at(idx)))

            if (.not. this%exists(child)) return
            idx = child

         end do
      end if

   end function __MANGLE(find_index)

   ! =======================
   !  rebalance
   ! =======================
   subroutine __MANGLE(rebalance)(this, pos, once)
      class(__set), intent(inout) :: this
      integer(kind=__gftl_size_kind), intent(in) :: pos
      logical, intent(in) :: once

      integer(kind=__gftl_size_kind) :: curr, child

      integer :: hl, hr, chl, chr, side, childside
      logical :: unbalanced

      integer(kind=__gftl_size_kind), pointer :: pLeft, pRight

      curr = pos

      do while (this%exists(curr))
         hl = 0
         hr = 0
         pLeft => this%lefts%at(curr)
         if (this%exists(pLeft)) hl = this%heights%at(pLeft)
         pRight => this%rights%at(curr)
         if (this%exists(pRight)) hr = this%heights%at(pRight)

         unbalanced = (abs(hl-hr) > 1)

         if (unbalanced) then
            side = merge(__LEFT, __RIGHT, hl>hr)
            child = this%get_child(curr, side)

            chl=0
            chr=0

            pLeft => this%lefts%at(child)
            if (this%exists(pLeft)) chl = this%heights%at(pLeft)
            pRight => this%rights%at(child)
            if (this%exists(pRight)) chr = this%heights%at(pRight)

            if (chr /= chl) then
               childside=merge(0, 1, chl>chr)
               if (side/=childside)                                                &
                    &            call this%rot(child,__MANGLE(opposite)(childside))
               call this%rot(curr, __MANGLE(opposite)(side))
            endif
         endif
         call this%update_height(curr)


         if (unbalanced.and.once) exit

         curr = this%parents%at(curr)

      end do

   end subroutine __MANGLE(rebalance)

   ! =======================
   !  erase_nonleaf
   ! =======================
   subroutine __MANGLE(erase_nonleaf)(this, pos, side)
      class(__set), intent(inout) :: this
      integer(kind=__gftl_size_kind), intent(inout) :: pos
      integer, intent(in) :: side

      integer(kind=__gftl_size_kind) :: parent, other, child0, child1,           &
           &      otherchild, otherparent

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

      if (this%exists(parent)) then
         call this%set_parent_child(parent, pos, other)
      else
         this%root = other
      endif
      call this%set_child(other, 1-side, child1)
      if (this%exists(child1)) call this%parents%set(child1, other)
      if (other == child0) then
         call this%rebalance(other, .false.)
      else
         call this%set_child(other, side, child0)
         call this%parents%set(child0, other)
         call this%set_child(otherparent, 1-side, otherchild)
         if (this%exists(otherchild)) then
            call this%parents%set(otherchild, otherparent)
         end if
         call this%rebalance(otherparent, .false.)
      endif

      ! declare this space available
      call this%parents%set(pos, this%next_free)
      this%next_free = pos

      pos = __UNINITIALIZED

   end subroutine __MANGLE(erase_nonleaf)

   ! =======================
   !  advpos
   ! =======================
   subroutine __MANGLE(advpos)(this, pos, dir)
      class(__set), target, intent(in) :: this
      integer(kind=__gftl_size_kind), intent(inout) :: pos
      integer, intent(in) :: dir   ! dir=1 forward, dir=0 backward

      integer(kind=__gftl_size_kind) :: prev
      integer(kind=__gftl_size_kind) :: child

      if (.not. this%exists(pos)) then
         if (.not. this%exists(this%root)) then
            return
         else
            pos = this%root
            do 
               child = this%get_child(pos, 1-dir)
               if (this%exists(child)) then
                  pos = child
               else
                  exit
               end if
            end do
         end if
      else
         child = this%get_child(pos, dir)
         if (this%exists(child)) then
            pos = child
            do 
               child = this%get_child(pos, __MANGLE(opposite)(dir))
               if (this%exists(child)) then
                  pos = child
               else
                  exit
               end if
            end do
         else
            prev = pos
            pos = this%parents%at(pos)

            do while (this%exists(pos))
               child = this%get_child(pos, dir)
               if (child /= prev) exit
               prev = pos
               pos = this%parents%at(pos)
            end do
         endif
      end if

   end subroutine __MANGLE(advpos)

   ! =======================
   !  rot - swap pos with one of its children
   ! =======================
   subroutine __MANGLE(rot)(this, pos, dir)
      class(__set), intent(inout) :: this
      integer(kind=__gftl_size_kind), intent(in) :: pos
      integer, intent(in) :: dir

      integer(kind=__gftl_size_kind) :: parent, child, grandchild

      parent = this%parents%at(pos)
      child = this%get_child(pos, __MANGLE(opposite)(dir))

      if (this%exists(child)) then
         grandchild = this%get_child(child, dir)
      else
         grandchild = __UNINITIALIZED
      end if

      if (this%exists(parent)) then
         call this%set_parent_child(parent, pos, child)
      else ! pos must be root; make the child root instead
         this%root = child
      endif

      ! 'child' is now my parent
      call this%parents%set(pos, child)

      ! 'grandchild' becomes now my child
      call this%set_child(pos, __MANGLE(opposite)(dir), grandchild)

      ! fix up child
      if (this%exists(child)) then
         call this%parents%set(child, parent)
         call this%set_child(child, dir, pos)
         ! and fix up grandchild
         if (this%exists(grandchild)) then
            call this%parents%set(grandchild, pos)
         end if
      end if

      call this%update_height(pos)

      if (this%exists(child)) call this%update_height(child)

   end subroutine __MANGLE(rot)

   !=========================================================================


   ! =======================
   !  value
   ! =======================
   function __MANGLE(value)(this) result(value)
      class(__iterator), target, intent(in) :: this
      __T_declare_result__, pointer :: value

      if (this%current == __UNINITIALIZED) then
         value => null()
      else
         value=>this%reference%items%at(this%current)
      end if

   end function __MANGLE(value)

   ! =======================
   !  next
   ! =======================
   subroutine __MANGLE(next)(this)
      class(__iterator), intent(inout) :: this

      call this%reference%advpos(this%current, 1)

   end subroutine __MANGLE(next)

   ! =======================
   !  prev
   ! =======================
   subroutine __MANGLE(prev)(this)
      class(__iterator), intent(inout) :: this

      call this%reference%advpos(this%current,0)

   end subroutine __MANGLE(prev)

   ! =======================
   !  equalIters
   ! =======================
   logical function __MANGLE(equalIters)(this, other)
      class(__iterator), intent(in) :: this, other

      __MANGLE(equalIters) = this%current == other%current

   end function __MANGLE(equalIters)

   ! =======================
   !  nequal
   ! =======================
   logical function __MANGLE(notEqualIters)(this, other)
      implicit none
      class(__iterator), intent(in) :: this, other

      __MANGLE(notEqualIters) = .not. (this == other)

   end function __MANGLE(notEqualIters)


   ! =======================
   !   deepCopy (assignment)
   ! =======================
   subroutine __MANGLE(deepCopy)(this, other)
      class (__set), target, intent(out) :: this
      class (__set), target, intent(in) :: other

      type (__iterator) :: iter
      __T_declare_result__, pointer :: ptr

      iter = other%begin()
      do while (iter /= other%end())
         ptr => iter%value()
         call this%insert(ptr)
         call iter%next()
      end do

      this%tsize = other%tsize

      return
   end subroutine __MANGLE(deepCopy)


   logical function __MANGLE(value_compare)(this, x, y) result(value_compare)
      class(__set), intent(in) :: this
      __T_declare_dummy__, intent(in) :: x
      __T_declare_dummy__, intent(in) :: y

      value_compare = __T_LT__(x,y)

      return
   end function __MANGLE(value_compare)


   ! Non type-bound procedures

   ! =======================
   !  equalSets
   ! =======================
   logical function __MANGLE(equal)(lhs, rhs) result(equal)
      type(__set), target, intent(in) :: lhs
      type(__set), target, intent(in) :: rhs

      type (__iterator) :: iter
      __T_declare_result__, pointer :: ptr

      equal = .false. ! unless
      if (lhs%size() /= rhs%size()) return

      iter = lhs%begin()
      do while (iter /= lhs%end())
         ptr => iter%value()
         if (rhs%count(ptr) == 0) then
            return
         end if
         call iter%next()
      end do

      equalSets = .true.

   end function __MANGLE(equal)


   ! =======================
   !  notEqualSets
   ! =======================
   logical function __MANGLE(not_equal)(lhs, rhs) result(not_equal)
      type(__set), intent(in) :: lhs
      type(__set), intent(in) :: rhs

      not_equal = .not. (lhs == rhs)

   end function __MANGLE(not_equal)




#include "templates/all_template_macros_undefs.inc"

#undef __UNINITIALIZED
#undef __LEFT
#undef __RIGHT

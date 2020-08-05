!--------------------------------------------------------------------
! Copyright © 2020 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------


#define __guard __set_guard
#include "parameters/T/copy_set_T_to_internal_T.inc"
#include "parameters/T/define_derived_macros.inc"


#include "parameters/T/define_derived_macros.inc"
#define __guard __set_guard

#define __LEFT 0
#define __RIGHT 1

#define __set_node __MANGLE(Node)

#include "templates/set/node_procedures.inc"

   ! Constructors
   function __MANGLE(new_set_empty)() result(s)
      type(__set) :: s

      s%tsize = 0
      s%root => null()
   end function __MANGLE(new_set_empty)


   function __MANGLE(new_set_copy)(x) result(s)
      type(__set) :: s
      type(__set), intent(in) :: x

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

   ! =======================
   !  empty()
   ! =======================
   logical function __MANGLE(empty)(this) result(empty)
      class(__set), intent(in) :: this

      empty = .not. associated(this%root)

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
   !  find
   ! =======================
   function __MANGLE(find)(this, value) result(find)
      class(__set), target, intent(in) :: this
      __T_declare_dummy__, intent(in) :: value
      type(__set_iterator) :: find

      find%tree => this
      find%node => this%find_node(value, .false.)

      if (associated(find%node)) then
         if (.not. __MANGLE(order_eq)(find%node%value,value)) then
            find%node => null()
         end if
      end if

      return
   end function __MANGLE(find)


   logical function __MANGLE(order_eq)(x, y) result(equal)
      __T_declare_dummy__, intent(in) :: x
      __T_declare_dummy__, intent(in) :: y

      equal = .not. __MANGLE(lessThan)(x,y) .and. .not. __MANGLE(lessThan)(y,x)
   end function __MANGLE(order_eq)


   ! =======================
   !  count
   ! =======================
   function __MANGLE(count)(this, value) result(count)
      integer(kind=__gftl_size_kind) :: count
      class(__set), target, intent(in) :: this
      __T_declare_dummy__, intent(in) :: value

      type (__set_iterator) :: i

      i = this%find(value)

      if (associated(i%node)) then
         count = 1
      else 
         count = 0
      end if

   end function __MANGLE(count)

   ! =======================
   !  clear
   ! =======================
   subroutine __MANGLE(clear)(this)
      class(__set), intent(inout) :: this

      if (associated(this%root)) deallocate(this%root)
      this%tsize=0
      return
   end subroutine __MANGLE(clear)

   ! =======================
   !  insert_single()
   ! =======================
   subroutine __MANGLE(insert_single)(this, value, unused, isNew, ref)
      class(__set), intent(inout) :: this
      __T_declare_dummy__, intent(in) :: value
      type (KeywordEnforcer), optional :: unused
      logical, optional, intent(out) :: isNew
      type(__set_node), pointer, optional, intent(out) :: ref
      type(__set_node), pointer :: new
      type(__set_node), pointer :: parent

      type(__set_node), pointer :: r

      if (present(unused)) print*,shape(unused)

      if (associated(this%root)) then
#ifdef __set_multi
         parent => this%find_node(value, .true.)
#else
         parent => this%find_node(value, .false.)
         if (__MANGLE(order_eq)(parent%value, value)) then
            if (present(ref)) then
               ref => parent
            else
               __T_FREE__(parent%value)
               __T_COPY__(parent%value, value)
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
         if (present(ref)) ref => new
         new%parent => parent
         __T_COPY__(new%value, value)
         call parent%set_child(merge(0, 1,                                        &
              &       __MANGLE(lessThan)(value, parent%value)),new)
         call this%rebalance(parent, .true.)
      else
         allocate(this%root)
         if (present(ref)) ref => this%root
         r => this%root
         __T_COPY__(r%value, value)
         if (present(isNew)) then
            isNew = .true.
         end if
      endif
      this%tsize = this%tsize + 1
      return
   end subroutine __MANGLE(insert_single)


#ifdef __T_listable__
   ! =======================
   !  insert_initializer_list()
   ! =======================
   subroutine __MANGLE(insert_initializer_list)(this, values)
      class(__set), intent(inout) :: this
      __T_declare_dummy__, intent(in) :: values(:)
      integer :: i

      do i = 1, size(values)
         call this%insert(values(i))
      end do

   end subroutine __MANGLE(insert_initializer_list)
#endif

   
   ! =======================
   !  insert_range()
   ! =======================
   subroutine __MANGLE(insert_range)(this, first, last)
      class(__set), intent(inout) :: this
      type(__set_iterator), intent(in) :: first
      type(__set_iterator), intent(in) :: last

      type(__set_iterator) :: iter

      iter = first
      do while (iter /= last)
         call this%insert(iter%at())
      end do

   end subroutine __MANGLE(insert_range)

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
#    define __COMPARE_COPY(x,y) __T_COPY__(x,y)
#    define __COMPARE_FREE(x) __T_FREE__(x)
#  endif

      logical function defaultLessThan(x, y) result(less)
         __compare_declare_dummy, intent(in) :: x
         __compare_declare_dummy, intent(in) :: y

         associate( wx => transfer(x,[1]), wy => transfer(y,[1]) )
           less = dictionaryLessThan1d(wx, wy)
         end associate

#  if !defined(__compare_type)
#    undef __compare_declare_dummy
#    undef __compare_declare_component
#    undef __COMPARE_COPY
#    undef __COMPARE_FREE
#  endif
      end function defaultLessThan
#endif

   end function __MANGLE(lessThan)






   ! =======================
   !  erase
   ! =======================
   subroutine __MANGLE(erase_one)(this, iter)
      class(__set), intent(inout) :: this
      type(__set_iterator), intent(inout) :: iter

      type (__set_iterator) :: last

      last = iter
      call last%next()
      call this%erase(iter, last)

   end subroutine __MANGLE(erase_one)


   ! =======================
   !  erase_multi
   ! =======================
   subroutine __MANGLE(erase_multi)(this, first, last)
      class(__set), intent(inout) :: this
      type(__set_iterator), intent(inout) :: first
      type(__set_iterator), intent(in) :: last
      type(__set_node), pointer :: pos, parent

      type (__set_iterator) :: iter

      iter = first
      do while (iter /= last)
         pos => iter%node
         call iter%next()
         if (associated(pos%right)) then
            call this%erase_nonleaf(pos, 1)
         else if (associated(pos%left)) then
            call this%erase_nonleaf(pos, 0)
         else
            parent => pos%parent
            if (associated(parent)) then
               call parent%set_child(parent%which_child(pos), null())
               call this%rebalance(parent, .false.)
            else
               this%root => null()
            endif
            deallocate(pos)
         endif
         this%tsize=this%tsize-1
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
      type(__set_iterator) :: it

      it=this%find(value)
      if (it/=this%end()) call this%erase(it)
      return
   end subroutine __MANGLE(remove)

   ! =======================
   !  begin
   ! =======================
   function __MANGLE(begin)(this) result(begin)
      class(__set), target, intent(in) :: this
      type(__set_iterator) :: begin

      begin%tree => this
      call begin%next()
      return
   end function __MANGLE(begin)

   ! =======================
   !  end
   ! =======================
   function __MANGLE(end)(this) result(end_)
      class(__set), target, intent(in) :: this
      type(__set_iterator) :: end_

      end_%tree => this
      return
   end function __MANGLE(end)

   ! =======================
   !  lower_bound
   ! =======================
   function __MANGLE(lower_bound)(this, value) result(lb)
      type(__set_iterator) :: lb
      class(__set), target, intent(in) :: this
      __T_declare_dummy__, intent(in) :: value
      __T_declare_result__, pointer :: p

      lb = this%begin()
      do while (lb /= this%end())
         p => lb%at()
         if (.not. __T_LT__(p, value)) exit
      end do

      return
   end function __MANGLE(lower_bound)

   ! =======================
   !  upper_bound
   ! =======================
   function __MANGLE(upper_bound)(this, value) result(ub)
      type(__set_iterator) :: ub
      class(__set), target, intent(in) :: this
      __T_declare_dummy__, intent(in) :: value

      __T_declare_result__, pointer :: p
      
      ub = this%end()
      do while (ub /= this%begin())
         call ub%prev()
         p => ub%at()
         if (.not. __T_LT__(value, p)) exit
      end do
            
      return
   end function __MANGLE(upper_bound)


!!$   ! =======================
!!$   !  write_formatted
!!$   ! =======================
!!$   recursive subroutine __MANGLE(write_formatted)(this, pos)
!!$      class(__set), intent(in) :: this
!!$      type(__set_node), pointer, intent(in), optional :: pos
!!$
!!$      if (present(pos)) then
!!$         if (associated(pos%left)) call this%write_formatted(pos%left)
!!$         write(*, *)pos%value, loc(pos), loc(pos%parent), loc(pos%left),  &
!!$              &       loc(pos%right), pos%height
!!$         if (associated(pos%right)) call this%write_formatted(pos%right)
!!$      else
!!$         write(*, *)'size=',this%getsize()
!!$         write(*, *)'root=',loc(this%root)
!!$         call this%write_formatted(this%root)
!!$      endif
!!$      return
!!$   end subroutine __MANGLE(write_formatted)
!!$
   ! =======================
   !  find_node
   ! =======================
   function __MANGLE(find_node)(this, value, last) result(find_node)
      type(__set_node), pointer :: find_node
      class(__set), target, intent(in) :: this
      __T_declare_dummy__, intent(in) :: value
#ifndef _pointer
!!$      target :: value
#endif
      logical, intent(in) :: last
      integer :: side

#ifdef _pointer
      __T_declare_result__, pointer :: p
#endif

      find_node => this%root
      if (associated(find_node)) then
         do
            if (.not. last .and. (                                           &
                 &      (__MANGLE(order_eq)(find_node%value,value)))) return
#ifdef _pointer
            p => value
            side=merge(0, 1, __MANGLE(lessThan)(p, find_node%value))
#else
            side=merge(0, 1, __MANGLE(lessThan)(value, find_node%value))
#endif
            if (.not.associated(find_node%child(side))) return
            find_node => find_node%child(side)
         end do
      end if

      return
   end function __MANGLE(find_node)

   ! =======================
   !  rebalance
   ! =======================
   subroutine __MANGLE(rebalance)(this, pos, once)
      class(__set), intent(inout) :: this
      type(__set_node), pointer, intent(in) :: pos
      logical, intent(in) :: once
      type(__set_node), pointer :: curr, child
      integer :: hl, hr, chl, chr, side, childside
      logical :: unbalanced

      curr => pos
      do while (associated(curr))
         hl=0
         hr=0
         if (associated(curr%left)) hl=curr%left%height
         if (associated(curr%right)) hr=curr%right%height
         unbalanced=abs(hl-hr)>1
         if (unbalanced) then
            side=merge(0, 1, hl>hr)
            child => curr%child(side)
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
         call curr%update_height()
         if (unbalanced.and.once) return
         curr => curr%parent
      end do
      return
   end subroutine __MANGLE(rebalance)

   ! =======================
   !  erase_nonleaf
   ! =======================
   subroutine __MANGLE(erase_nonleaf)(this, pos, side)
      class(__set), intent(inout) :: this
      type(__set_node), pointer, intent(inout) :: pos
      integer, intent(in) :: side
      type(__set_node), pointer :: parent, other, child0, child1
      type(__set_node), pointer :: otherchild, otherparent

      parent => pos%parent
      other => pos
      call this%advpos(other, side)
      child0 => pos%child(side)
      child1 => pos%child(1-side)
      otherchild => other%child(side)
      otherparent => other%parent
      other%parent => parent
      if (associated(parent)) then
         call parent%set_child(parent%which_child(pos), other)
      else
         this%root => other
      endif
      call other%set_child(1-side, child1)
      if (associated(child1)) child1%parent => other
      if (associated(other, target=child0)) then
         call this%rebalance(other, .false.)
      else
         call other%set_child(side, child0)
         child0%parent => other
         call otherparent%set_child(1-side, otherchild)
         if (associated(otherchild)) otherchild%parent => otherparent
         call this%rebalance(otherparent, .false.)
      endif
      pos%left => null()
      pos%right => null()
      deallocate(pos)
      return
   end subroutine __MANGLE(erase_nonleaf)

   ! =======================
   !  advpos
   ! =======================
   subroutine __MANGLE(advpos)(this, pos, dir)
      class(__set), target, intent(in) :: this
      type(__set_node), pointer, intent(inout) :: pos
      integer, intent(in) :: dir   ! dir=1 forward, dir=0 backward
      type(__set_node), pointer :: prev

      if (.not.associated(pos)) then
         if (.not.associated(this%root)) return
         pos => this%root
         do while (associated(pos%child(1-dir)))
            pos => pos%child(1-dir)
         end do
      else if (associated(pos%child(dir))) then
         pos => pos%child(dir)
         do while (associated(pos%child(1-dir)))
            pos => pos%child(1-dir)
         end do
      else
         prev => pos
         pos => pos%parent
         do while (associated(pos))
            if (.not.associated(pos%child(dir), prev)) exit
            prev => pos
            pos => pos%parent
         end do
      endif
      return
   end subroutine __MANGLE(advpos)

   ! =======================
   !  rot
   ! =======================
   subroutine __MANGLE(rot)(this, pos, dir)
      class(__set), intent(inout) :: this
      type(__set_node), pointer, intent(in) :: pos
      integer, intent(in) :: dir
      type(__set_node), pointer :: parent, child, grandchild => null()

      parent => pos%parent
      child => pos%child(1-dir)
      if (associated(child)) grandchild => child%child(dir)
      if (associated(parent)) then
         call parent%set_child(parent%which_child(pos), child)
      else
         this%root => child
      endif
      pos%parent => child
      call pos%set_child(1-dir, grandchild)
      if (associated(child)) then
         child%parent => parent
         call child%set_child(dir, pos)
         if (associated(grandchild)) grandchild%parent => pos
      endif
      call pos%update_height()
      if (associated(child)) call child%update_height()
      return
   end subroutine __MANGLE(rot)

   logical function __MANGLE(value_compare)(this, x, y) result(value_compare)
      class(__set), intent(in) :: this
      __T_declare_dummy__, intent(in) :: x
      __T_declare_dummy__, intent(in) :: y

      __UNUSED_DUMMY(this)
      value_compare = __T_LT__(x,y)

      return
   end function __MANGLE(value_compare)


   ! =======================
   !  equal
   ! =======================
   logical function __MANGLE(equal)(a, b) result(equal)
      type(__set), target, intent(in) :: a
      type(__set), target, intent(in) :: b

      type (__set_iterator) :: iter
      __T_declare_result__, pointer :: ptr

      equal = .false. ! unless
#if  !defined(__INTEL_COMPILER) | !(defined(_string) & !defined(_string_deferred))
      if (a%size() /= b%size()) return

      iter = a%begin()
      do while (iter /= a%end())
         ptr => iter%at()
         if (b%count(ptr) == 0) then
            return
         end if
         call iter%next()
      end do

      equal = .true.
#endif
   end function __MANGLE(equal)


   ! =======================
   !  not_equal
   ! =======================
   logical function __MANGLE(not_equal)(a, b) result(not_equal)
      type(__set), intent(in) :: a
      type(__set), intent(in) :: b

      not_equal = .not. (a == b)

   end function __MANGLE(not_equal)


   ! =======================
   !   deep_copy (assignment)
   ! =======================
   subroutine __MANGLE(deep_copy)(this, other)
      class(__set), target, intent(out) :: this
      class(__set), target, intent(in) :: other

      type(__set_iterator) :: iter
      __T_declare_result__, pointer :: ptr

#if  !defined(__INTEL_COMPILER) | !(defined(_string) & !defined(_string_deferred))
      iter = other%begin()
      do while (iter /= other%end())
         ptr => iter%at()
         call this%insert(ptr)
         call iter%next()
      end do
#endif
      this%tsize = other%tsize

   end subroutine __MANGLE(deep_copy)

   ! =======================
   !  finalize_set()
   ! =======================
   recursive subroutine __MANGLE(finalize_set)(this)
      type(__set), intent(inout) :: this

      if (associated(this%root)) deallocate(this%root)

      return
   end subroutine __MANGLE(finalize_set)


   subroutine __MANGLE(merge)(this, source)
      class(__set), intent(inout) :: this
      type(__set), intent(in) :: source

      type(__set_iterator) :: iter

      iter = source%begin()
      do while (iter /= source%end())
         
         if (this%count(iter%at()) == 0) then
         end if
         call iter%next()
      end do
   end subroutine __MANGLE(merge)

#include "templates/set/iterator_procedures.inc"


#undef __LEFT 0
#undef __RIGHT 1
#undef __set_node

   ! undef derived template parameters
#include "parameters/T/undef_derived_macros.inc"
   ! undef internal template parameters
#undef __guard
#include "parameters/T/undef_internal.inc"

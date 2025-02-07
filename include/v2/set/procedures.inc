#ifdef _DEPTH_
#  undef _DEPTH_
#endif
#define _DEPTH_ *

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

#define __guard __set_guard

#define __LEFT 0
#define __RIGHT 1

#define __base_node __MANGLE(BaseNode)
#define __set_node __MANGLE(Node)

#include "set/node_procedures.inc"

   ! Constructors
   function __MANGLE(new_set_empty)() result(s)
      type(__set) :: s

      s%tsize = 0
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

      empty = .not. allocated(this%root)

   end function __MANGLE(empty)

   ! =======================
   !  size
   ! =======================
   function __MANGLE(size)(this) result(size)
      integer(kind=GFTL_SIZE_KIND) :: size
      class(__set), intent(in) :: this

      size = this%tsize

   end function __MANGLE(size)

   ! =======================
   !  max_size()
   ! =======================
   pure function __MANGLE(max_size)() result(res)
      integer(kind=GFTL_SIZE_KIND) :: res

      integer(kind=GFTL_SIZE_KIND) :: index

      res = huge(index)

      return
   end function __MANGLE(max_size)


   ! =======================
   !  find
   ! =======================
   function __MANGLE(find)(this, value) result(find)
      type(__set_iterator) :: find
      class(__set), target, intent(in) :: this
      __T_declare_dummy__, intent(in) :: value

      find%tree => this
      find%node => this%find_node(value, .false.)

      if (associated(find%node)) then
         if (.not. __MANGLE(order_eq)(find%node%get_value(),value)) then
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
      integer(kind=GFTL_SIZE_KIND) :: count
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
   recursive subroutine __MANGLE(clear)(this)
      class(__set), intent(inout) :: this

      if (allocated(this%root)) deallocate(this%root)

      this%tsize = 0
      return
         
   end subroutine __MANGLE(clear)

   ! =======================
   !  insert_single()
   ! =======================
   recursive subroutine __MANGLE(insert_single)(this, value, unused, is_new, iter)
      class(__set), target, intent(inout) :: this
      __T_declare_dummy__, intent(in) :: value
      type (KeywordEnforcer), optional :: unused
      logical, optional, intent(out) :: is_new
      type(__set_iterator), optional, intent(out) :: iter
      type(__set_node), target, allocatable :: new
      type(__set_node), pointer :: parent

      class(__set_node), pointer :: r

      integer, save :: depth = 0

      depth = depth + 1

      if (present(iter)) iter%tree => this


      if (allocated(this%root)) then
#ifdef __set_multi
         parent => this%find_node(value, .true.)
#else
         parent => this%find_node(value, .false.)
         if (__MANGLE(order_eq)(parent%get_value(), value)) then
            if (present(iter)) then
               iter%node => parent
            else
               __T_FREE__(parent%value)
               call parent%set_value(value)
            endif
            if (present(is_new)) then
               is_new = .false.
            end if
            depth = depth - 1
            return
         endif
#endif
         if (present(is_new)) then
            is_new = .true.
         end if

         allocate(new)
         if (present(iter)) iter%node => new
         call new%set_parent(parent)
         __T_COPY__(new%value, value)

         call parent%set_child(merge(__LEFT, __RIGHT, __T_LT__(value, parent%get_value())), new)
         if (allocated(new)) error stop 'should be deallocated'
         call this%rebalance(parent, .true.)
      else
         allocate(__set_node :: this%root)
         if (present(iter)) iter%node => this%root%to_node()
         select type (q => this%root)
         type is (__set_node)
            r => q
         end select
         call r%set_value(value)
         if (present(is_new)) then
            is_new = .true.
         end if
      endif
      this%tsize = this%tsize + 1

      depth = depth - 1

      return

      if (present(unused)) print*,shape(unused)

   end subroutine __MANGLE(insert_single)


#ifdef __T_listable__
   ! =======================
   !  insert_initializer_list()
   ! =======================
   recursive subroutine __MANGLE(insert_initializer_list)(this, values)
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
   recursive subroutine __MANGLE(insert_range)(this, first, last)
      class(__set), intent(inout) :: this
      type(__set_iterator), intent(in) :: first
      type(__set_iterator), intent(in) :: last

      type(__set_iterator) :: iter

      iter = first
      do while (iter /= last)
         call this%insert(iter%of())
         call iter%next()
      end do

   end subroutine __MANGLE(insert_range)

   ! =======================
   !  insert_with_hint()
   ! =======================
   ! Currently a very sub-optimal implementation.

   recursive subroutine __MANGLE(insert_single_with_hint)(this, hint, value, unused, iter)
      class(__set), target, intent(inout) :: this
      type(__set_iterator), intent(in) :: hint
      __T_declare_dummy__, intent(in) :: value
      type (KeywordEnforcer), optional :: unused
      type(__set_iterator), optional, intent(out) :: iter

      call this%insert(value, iter=iter)

      __UNUSED_DUMMY(unused)
      __UNUSED_DUMMY(hint)
   end subroutine __MANGLE(insert_single_with_hint)




   logical function __MANGLE(lessThan)(x, y) result(less)
      __T_declare_dummy__, intent(in) :: x
      __T_declare_dummy__, intent(in) :: y

#if defined(__set_LT)
      less = __set_LT__(x,y)
#else
      less = __T_LT__(x,y)
#endif

   contains

      ! TODO:  possibly this procedure should be inside some sort of #ifdef
      logical function dictionaryLessThan1d(x, y) result(less)
         integer, intent(in) :: x(:)
         integer, intent(in) :: y(:)

         integer(kind=GFTL_SIZE_KIND) :: i, n

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
    end function __MANGLE
#endif

 end function __MANGLE(lessThan)






 ! =======================
 !  erase
 ! =======================
 function __MANGLE(erase_iter)(this, position) result(iter)
    type(__set_iterator) :: iter
    class(__set), target, intent(inout) :: this
    type(__set_iterator), intent(in) :: position

    type (__set_iterator) :: last

    last = position
    call last%next()
    iter = this%erase(position, last)

 end function __MANGLE(erase_iter)


 function __MANGLE(erase_value)(this, value) result(n)
    integer(kind=GFTL_SIZE_KIND) :: n
    class(__set), target, intent(inout) :: this
    __T_declare_dummy__, intent(in) :: value

    type(__set_iterator) :: iter

    iter = this%find(value)
    if (iter /= this%end()) then
       iter = this%erase(iter)
#ifdef __set_multi
       error - unhandled case for multiset
#else
       n = 1
#endif
    else
       n = 0
    end if
 end function __MANGLE(erase_value)

 ! =======================
 !  erase_multi
 ! =======================
 function __MANGLE(erase_range)(this, first, last) result(next_iter)
    type(__set_iterator) :: next_iter
    class(__set), intent(inout) :: this
    type(__set_iterator), intent(in) :: first
    type(__set_iterator), intent(in) :: last
    type(__set_node), pointer :: parent
    type(__set_node), pointer :: pos

    type (__set_iterator) :: iter

    next_iter = last

    iter = first
    do while (iter /= last)
       pos => iter%node
       call iter%next()
       if (pos%has_child(__RIGHT)) then
          call this%erase_nonleaf(pos, __RIGHT)
       else if (pos%has_child(__LEFT)) then
          call this%erase_nonleaf(pos, __LEFT)
       else
          parent => pos%get_parent()
          if (associated(parent)) then
             call parent%deallocate_child(parent%which_child(pos))
             call this%rebalance(parent, .false.)
          else
             deallocate(this%root)
          endif
       endif
       this%tsize=this%tsize-1
    end do

    return
 end function __MANGLE(erase_range)

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
 !  ftn_begin
 ! =======================
 function __MANGLE(ftn_begin)(this) result(begin)
    class(__set), target, intent(in) :: this
    type(__set_iterator) :: begin

    begin%tree => this

    return
 end function __MANGLE(ftn_begin)

 ! =======================
 !  end
 ! =======================
 function __MANGLE(ftn_end)(this) result(end_)
    class(__set), target, intent(in) :: this
    type(__set_iterator) :: end_

    type(__set_iterator) :: tmp

    ! brute force implementation for now
    end_ = next(this%begin(), this%size()-1)

    return
 end function __MANGLE(ftn_end)

 ! =======================
 !  lower_bound
 ! =======================
 function __MANGLE(lower_bound)(this, value) result(lb)
    type(__set_iterator) :: lb
    class(__set), target, intent(in) :: this
    __T_declare_dummy__, intent(in) :: value

    type(__set_node), pointer :: node

    lb%tree => this
    node => this%find_node(value, .false.)
    lb%node => node

    if (__T_LT__(node%value, value)) then
       if (lb /= this%end()) call lb%next()
    end if

    return
 end function __MANGLE(lower_bound)

 ! =======================
 !  upper_bound
 ! =======================
 function __MANGLE(upper_bound)(this, value) result(ub)
    type(__set_iterator) :: ub
    class(__set), target, intent(in) :: this
    __T_declare_dummy__, intent(in) :: value

    type(__set_node), pointer :: node

    ub%tree => this
    node => this%find_node(value, .false.)
    ub%node => node

    if (.not. (__T_LT__(value, node%value))) then
       if (ub /= this%end()) call ub%next()
    end if

    return
 end function __MANGLE(upper_bound)


 ! =======================
 !  find_node
 ! =======================
 function __MANGLE(find_node)(this, value, last) result(find_node)
    type(__set_node), pointer :: find_node
    class(__set), target, intent(in) :: this
    __T_declare_dummy__, intent(in) :: value
    logical, intent(in) :: last
    integer :: side

    if (.not. allocated(this%root)) then
       find_node => null()
       return
    end if

    find_node => this%root%to_node()
    if (associated(find_node)) then
       do
          if (.not. last .and. (                                           &
               &     (__MANGLE(order_eq)(find_node%get_value(),value)))) then
             return
          end if
          side=merge(__LEFT, __RIGHT, __MANGLE(lessThan)(value, find_node%get_value()))
          if (.not.associated(find_node%get_child(side))) return
          find_node => find_node%get_child(side)
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
      integer :: hl, hr, chl, chr, side, child_side
      logical :: unbalanced

      curr => pos
      do while (associated(curr))
         hl=0
         hr=0
         if (curr%has_child(__LEFT)) hl=curr%left%get_height()
         if (curr%has_child(__RIGHT)) hr=curr%right%get_height()
         unbalanced=abs(hl-hr)>1
         if (unbalanced) then
            side = merge(__LEFT, __RIGHT, hl > hr)
            child => curr%get_child(side)
            chl = 0
            chr = 0
            if (child%has_child(__LEFT)) chl = child%left%get_height()
            if (child%has_child(__RIGHT)) chr = child%right%get_height()
            if (chr /= chl) then
               child_side=merge(__LEFT, __RIGHT, chl > chr)
               if (side /= child_side) call this%rot(child, 1-child_side)
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
      class(__base_node), allocatable :: tmp_other, tmp_pos
      
      parent => pos%parent
      other => pos
      call this%advpos(other, side)
      child0 => pos%get_child(side)
      child1 => pos%get_child(1-side)
      otherchild => other%get_child(side)
      otherparent => other%parent

      select case (other%which_side_am_i())
      case (__LEFT)
         call move_alloc(from=otherparent%left, to=tmp_other)
      case (__RIGHT)
         call move_alloc(from=otherparent%right, to=tmp_other)
      end select

      call tmp_other%set_parent(parent) 
      if (associated(parent)) then
         select case (pos%which_side_am_i())
         case (__LEFT)
            call move_alloc(from=parent%left, to=tmp_pos)
            call move_alloc(from=tmp_other, to=parent%left)
         case (__RIGHT)
            call move_alloc(from=parent%right, to=tmp_pos)
            call move_alloc(from=tmp_other, to=parent%right)
         end select
      else
         call move_alloc(from=this%root, to=tmp_pos)
         call move_alloc(from=tmp_other, to=this%root)
      endif

      if (associated(child1)) then
         select type (q => tmp_pos)
         type is (__set_node)
            select case(side)
            case (__LEFT)
               call move_alloc(from=q%right, to=other%right)
               call other%right%set_parent(other)
            case (__RIGHT)
               call move_alloc(from=q%left, to=other%left)
               call other%left%set_parent(other)
            end select
         end select
      end if

      if (associated(other, target=child0)) then ! degenerate
         call this%rebalance(other, .false.)
      else
         select type (q => tmp_pos)
         type is (__set_node)
            select case (side)
            case (__LEFT)
               if (associated(otherchild)) call move_alloc(from=other%left, to=otherparent%right)
               call move_alloc(from=q%left, to=other%left)
               call other%left%set_parent(other)
            case (__RIGHT)
               if (associated(otherchild)) call move_alloc(from=other%right, to=otherparent%left)
               call move_alloc(from=q%right, to=other%right)
               call other%right%set_parent(other)
            end select
         end select
         if (associated(otherchild)) then
            call otherchild%set_parent(otherparent)
         end if
         call this%rebalance(otherparent, .false.)
      end if

      ! pos has been gutted.  We can delete the rest.
      deallocate(tmp_pos)
      return
   end subroutine __MANGLE(erase_nonleaf)

   ! =======================
   !  advpos
   ! =======================
   recursive subroutine __MANGLE(advpos)(this, pos, dir)
      class(__set), target, intent(in) :: this
      type(__set_node), pointer, intent(inout) :: pos
      integer, intent(in) :: dir   ! dir=1 forward, dir=0 backward
      type(__set_node), pointer :: prev

      if (.not.associated(pos)) then
         if (.not. allocated(this%root)) return
         pos => this%root%to_node()
         do while (associated(pos%get_child(1-dir)))
            pos => pos%get_child(1-dir)
         end do
      else if (associated(pos%get_child(dir))) then
         pos => pos%get_child(dir)
         do while (associated(pos%get_child(1-dir)))
            pos => pos%get_child(1-dir)
         end do
      else
         prev => pos
         pos => pos%parent
         do while (associated(pos))
            if (.not.associated(pos%get_child(dir), prev)) exit
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
      type(__set_node), pointer, intent(inout) :: pos
      integer, intent(in) :: dir
      type(__set_node), pointer :: parent, child, grandchild => null()

      class(__base_node), allocatable :: A, B, C
      integer :: pos_side
      
      parent => pos%parent

      if (associated(parent)) then
         pos_side = pos%which_side_am_i()
         select case (pos_side)
         case (__LEFT)
            call move_alloc(from=parent%left, to=A)
         case (__RIGHT)
            call move_alloc(from=parent%right, to=A)
         end select
      else
         call move_alloc(from=this%root, to=A)
      endif

      child => pos%get_child(1-dir)
      if (associated(child)) then
         select case (1-dir)
         case (__LEFT)
            call move_alloc(from=pos%left, to=B)
         case (__RIGHT)
            call move_alloc(from=pos%right, to=B)
         end select
      else
         error stop "isn't there always a child for rot?"
      end if
      
      grandchild => child%get_child(dir)
      if (associated(grandchild)) then
         select case (dir)
         case (__LEFT)
            call move_alloc(from=child%left, to=C)
         case (__RIGHT)
            call move_alloc(from=child%right, to=C)
         end select
      end if

      if (associated(grandchild)) then
         select type (A)
         type is (__set_node)
            select case (1-dir)
            case (__LEFT)
               call move_alloc(from=C, to=A%left)
            case (__RIGHT)
               call move_alloc(from=C, to=A%right)
            end select
         end select
         call grandchild%set_parent(pos)
      end if

      if (associated(child)) then
         select type (B)
         type is (__set_node)
            select case (dir)
            case (__LEFT)
               call move_alloc(from=A, to=B%left)
            case (__RIGHT)
               call move_alloc(from=A, to=B%right)
            end select
         end select
         call pos%set_parent(child)
      end if
      
      if (associated(parent)) then
         select case (pos_side)
         case (__LEFT)
            call move_alloc(from=B, to=parent%left)
         case (__RIGHT)
            call move_alloc(from=B, to=parent%right)
         end select
      else
         call move_alloc(from=B, to=this%root)
      endif
      call child%set_parent(parent)

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

      type (__set_iterator) :: iter_a
      type (__set_iterator) :: iter_b
      __T_declare_result__, pointer :: ptr_a
      __T_declare_result__, pointer :: ptr_b

      equal = .false. ! unless
      if (a%size() /= b%size()) return

      iter_a = a%begin()
      iter_b = b%begin()
      do while (iter_a /= a%end())
         ptr_a => iter_a%of()
         ptr_b => iter_b%of()

         if (.not. __MANGLE(order_eq)(ptr_a,ptr_b)) return

         call iter_a%next()
         call iter_b%next()
      end do

      equal = .true.

   end function __MANGLE(equal)


   ! =======================
   !  not_equal
   ! =======================
   logical function __MANGLE(not_equal)(a, b) result(not_equal)
      type(__set), intent(in) :: a
      type(__set), intent(in) :: b

      not_equal = .not. (a == b)

   end function __MANGLE(not_equal)

   ! ======================
   !  operator(<) less_than()
   ! ======================
   logical function __MANGLE(less_than)(a,b) result(lt)
      type(__set), intent(in) :: a
      type(__set), intent(in) :: b

      type (__set_iterator) :: iter_a
      type (__set_iterator) :: iter_b
      __T_declare_result__, pointer :: ptr_a
      __T_declare_result__, pointer :: ptr_b

      iter_a = a%begin()
      iter_b = b%begin()
      do while (iter_a /= a%end() .and. iter_b /= b%end())
         ptr_a => iter_a%of()
         ptr_b => iter_b%of()

         ! a < b ?
         lt = __T_LT__(ptr_a, ptr_b)
         if (lt) return

         ! b < a ?
         lt = __T_LT__(ptr_b, ptr_a)
         if (lt) return
         
         ! items equal so continue

         call iter_a%next()
         call iter_b%next()
      end do

      lt = (a%size() < b%size())
      
      return
   end function __MANGLE(less_than)


   ! ======================
   !  operator(<=) - less_than_or_equal() 
   ! ======================
   logical function __MANGLE(less_than_or_equal)(a,b) result(le)
      type(__set), intent(in) :: a
      type(__set), intent(in) :: b

      le = .not. (b < a)
      return
   end function __MANGLE(less_than_or_equal)

   ! ======================
   !  operator(>) - greater_than()
   ! ======================
   logical function __MANGLE(greater_than)(a,b) result(gt)
      type(__set), intent(in) :: a
      type(__set), intent(in) :: b

      gt = (b < a)
      return
   end function __MANGLE(greater_than)

   ! ======================
   !  operator(>=) - greater_than_or_equal() 
   ! ======================
   logical function __MANGLE(greater_than_or_equal)(a,b) result(ge)
      type(__set), intent(in) :: a
      type(__set), intent(in) :: b

      ge = .not. (a < b)
      return
   end function __MANGLE(greater_than_or_equal)

   ! =======================
   !   deep_copy (assignment)
   ! =======================
   recursive subroutine __MANGLE(deep_copy)(this, other)
      class(__set), target, intent(out) :: this
      class(__set), target, intent(in) :: other

      type(__set_iterator) :: iter
      __T_declare_result__, pointer :: ptr

      integer, save :: depth = 0
      integer :: counter

      if (.not. allocated(other%root)) return

      depth = depth + 1

      associate ( b => other%begin(), e => other%end() )
        iter = b
        counter = 0
        do while (iter /= e)
           counter = counter + 1
           ptr => iter%of()
           if (.not. associated(ptr)) then
              error stop 'bad ptr'
           end if
           call this%insert(ptr)
           call iter%next()
        end do
      end associate

      this%tsize = other%tsize
      depth = depth - 1
      
   end subroutine __MANGLE(deep_copy)

#ifdef __T_listable__
   subroutine __MANGLE(copy_list)(this, il)
      class(__set), intent(out) :: this
      __T_declare_dummy__, intent(in) :: il(:)

!!$      this = __set ()
      call this%insert(il)

   end subroutine __MANGLE(copy_list)
#endif
   ! Suboptimal implementation
   recursive subroutine __MANGLE(merge)(this, source)
      class(__set), intent(inout) :: this
      type(__set), target, intent(inout) :: source

      type(__set_iterator) :: iter

      iter = source%begin()
      do while (iter /= source%end())
         if (this%count(iter%of()) == 0) then
            
            call this%insert(iter%of())
            iter = source%erase(iter)
         else
            ! already in result
            call iter%next()
         end if
      end do
   end subroutine __MANGLE(merge)


   subroutine __MANGLE(write_formatted)(this, unit, iotype, v_list, iostat, iomsg)
    class(__set), intent(in) :: this
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    iostat = 0

#ifdef __T_name__
    write(unit,'(a)') 'Set<' // __T_declare_string__ // '>'
#else
    write(unit,'(a)') 'Set<' // 'unknown' // '>'
#endif
    write(unit,'(a)') new_line('a')
    write(unit,'(4x,a10,1x,i0)') 'size: ',this%size()

    __UNUSED_DUMMY(iomsg)
    __UNUSED_DUMMY(iotype)
    __UNUSED_DUMMY(v_list)
   end subroutine __MANGLE(write_formatted)


   ! =======================
   !  swap
   ! =======================
   subroutine __MANGLE(swap)(this, x)
      class(__set), target, intent(inout) :: this
      type(__set), target, intent(inout) :: x

      class(__base_node), allocatable :: tmp
      integer(kind=GFTL_SIZE_KIND) :: tsize

      call move_alloc(from=this%root, to=tmp)
      call move_alloc(from=x%root, to=this%root)
      call move_alloc(from=tmp, to=x%root)

      tsize = this%tsize
      this%tsize = x%tsize
      x%tsize = tsize

      return
   end subroutine __MANGLE(swap)

   recursive logical function __MANGLE(verify)(this) result(verify)
      class (__set), target, intent(in) :: this
      
         integer :: counter, sz
         type(__set_iterator) :: iter


      if (.not. allocated(this%root)) then
         verify = .true.
         return
      else
         select type (q => this%root)
         type is (__set_node)
            verify = verify_node(q)
         end select
         if (.not. verify) return
         
         sz = this%size()
         associate ( b => this%begin(), e => this%end())
           iter = b
           counter = 0
           do while (iter /= e)
              counter = counter + 1
              call iter%next()
           end do
         end associate
         verify = (counter == sz)
      end if

   contains

      recursive logical function verify_node(node) result(verify)
         type(__set_node), target, intent(in) :: node

         verify = verify_child(node, __LEFT) .and. verify_child(node, __RIGHT)

      end function verify_node

      recursive logical function verify_child(node, dir) result(verify)
         type(__set_node), target, intent(in) :: node
         integer, intent(in) :: dir

         type(__set_node), pointer :: ch
         

         if (node%has_child(dir)) then
            ch => node%get_child(dir)
            if (.not. associated(ch%parent, node)) then
               verify = .false.
               return
            end if
            verify = verify_node(ch)
         else
            verify = .true. ! null case
         end if

      end function verify_child

      

   end function __MANGLE(verify)


#ifdef __DEBUG__   
   subroutine dump_at_line(this, line)
      type(__set), target, intent(in) :: this
      integer, intent(in) :: line

      type(__set_node), pointer :: node
      class(__base_node), pointer :: lch, rch

      lch => this%root%get_child(__LEFT)
      rch => this%root%get_child(__RIGHT)
!!$      print*,'At line: ', line, lch%get_height(), rch%get_height()
      if (this%size() > 0) then
         node => this%root%to_node()
         call dump(node)
      end if
      print*
   end subroutine dump_at_line

   recursive subroutine dump(node)
      type(__set_node), target, intent(in) :: node
      type(__set_node), pointer :: left, right
      type(__set_node), pointer :: parent

!!$      if (associated(node%parent)) then
!!$         write(*,'(i0,":",i0)',advance='no') node%value, node%parent%value
!!$      else
         write(*,'(i0,1x)',advance='no') node%value
!!$      end if
      if (node%has_child(__LEFT) .or. node%has_child(__RIGHT)) then
         write(*,'(" (")', advance='no')
         if (node%has_child(__LEFT)) then
            call dump(node%get_child(__LEFT))
         else
            write(*,'(" *")', advance='no')
         end if
         write(*,'(" ")', advance='no')
         if (node%has_child(__RIGHT)) then
            call dump(node%get_child(__RIGHT))
         else
            write(*,'(" * ")', advance='no')
         end if
         write(*,'(" )")', advance='no')
      end if

   end subroutine dump
#endif      

#include "set/iterator_procedures.inc"


#undef __LEFT
#undef __RIGHT
#undef __set_node
#undef __base_node

   ! undef derived template parameters
#include "parameters/T/undef_derived_macros.inc"
   ! undef internal template parameters
#undef __guard
#include "parameters/T/undef_internal.inc"

#include "parameters/T/copy_set_T_to_algorithm_T.inc"
#define __algorithm_guard __IDENTITY(__set_guard)
#define __algorithm_container __set
#define __algorithm_iterator __set_iterator
#include "algorithms/procedures.inc"   
#undef __algorithm_container
#undef __algorithm_iterator
#undef __algorithm_T
#undef __algorithm_guard
#include "parameters/T/undef_algorithm_T.inc"
   

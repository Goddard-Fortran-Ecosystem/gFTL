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

#if (defined(__guard))
#  undef __guard
#endif
#define __guard __alt_set_guard

#define __LEFT 0
#define __RIGHT 1

#include "parameters/T/copy_alt_set_T_to_internal_T.inc"
#include "parameters/T/define_derived_macros.inc"

! Vector spec   
#define __vector __MANGLE(Vector)
#define __vector_iterator ItemVectorIterator
#define __vector_riterator ItemVectorRIterator
#define __vector_T __alt_set_T
#define __vector_guard __IDENTITY(__alt_set_guard)__IDENTITY(Item)

#include "vector/procedures.inc"

#undef __vector_guard
#undef __vector_T
#undef __vector_riterator
#undef __vector_iterator
#undef __vector

#define __vector __MANGLE(Vector)
#define __vector_iterator IndexVectorIterator
#define __vector_riterator IndexVectorRIterator
#define __vector_T __INTEGER64
#define __vector_guard __IDENTITY(__alt_set_guard)__IDENTITY(Index)

#include "vector/procedures.inc"

#undef __vector_guard
#undef __vector_T
#undef __vector_rterator
#undef __vector_iterator
#undef __vector

#define __guard __alt_set_guard

#include "parameters/T/copy_alt_set_T_to_internal_T.inc"
#include "parameters/T/define_derived_macros.inc"



   ! Constructors
   function __MANGLE(new_set_empty)() result(s)
      type(__alt_set) :: s

      s%tsize = 0
   end function __MANGLE(new_set_empty)


   function __MANGLE(new_set_copy)(x) result(s)
      type(__alt_set) :: s
      type(__alt_set), intent(in) :: x

      s = x
   end function __MANGLE(new_set_copy)

#ifdef __T_listable__
   function __MANGLE(new_set_initializer_list)(il) result(s)
      type (__alt_set) :: s
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
      class(__alt_set), intent(in) :: this

      empty = (.not. __MANGLE(exists)(this%root))

   end function __MANGLE(empty)

   ! =======================
   !  size
   ! =======================
   function __MANGLE(size)(this) result(size)
      integer(kind=GFTL_SIZE_KIND) :: size
      class(__alt_set), intent(in) :: this

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
      type(__alt_set_iterator) :: find
      class(__alt_set), target, intent(in) :: this
      __T_declare_dummy__, intent(in) :: value

      find%tree => this

      associate (c => find%current)
        c = this%find_index(value, .false.)

        if (__MANGLE(exists)(c)) then
           if (.not. __MANGLE(order_eq)(this%items%of(c), value)) then
              c = UNINITIALIZED
           end if
        end if
      end associate

      return
   end function __MANGLE(find)


! =======================
!  find_index
! =======================
   function __MANGLE(find_index)(this, value, last) result(idx)
      integer(kind=GFTL_SIZE_KIND) :: idx
      class(__alt_set), target, intent(in) :: this
      __T_declare_dummy__, intent(in) :: value
      logical, intent(in) :: last

      integer (kind=GFTL_SIZE_KIND) :: child

      idx = this%root
      if (__MANGLE(exists)(idx)) then
         do
            if (.not. last .and. (                                           &
     &         __MANGLE(order_eq)(this%items%of(idx),value))) return
            child=merge(this%lefts%of(idx), this%rights%of(idx),              &
     &         __MANGLE(v_lessThan)(value, this%items%of(idx)))
          
            if (.not. __MANGLE(exists)(child)) return
            idx = child
         end do
      end if

   end function __MANGLE(find_index)


   logical function __MANGLE(order_eq)(x, y) result(equal)
      __T_declare_dummy__, intent(in) :: x
      __T_declare_dummy__, intent(in) :: y

      equal = .not. __MANGLE(v_lessThan)(x,y) .and. .not. __MANGLE(v_lessThan)(y,x)
   end function __MANGLE(order_eq)


   ! =======================
   !  count
   ! =======================
   function __MANGLE(count)(this, value) result(count)
      integer(kind=GFTL_SIZE_KIND) :: count
      class(__alt_set), target, intent(in) :: this
      __T_declare_dummy__, intent(in) :: value

      type (__alt_set_iterator) :: i

      i = this%find(value)

      count = 0
      if (__MANGLE(exists)(i%current)) then
         count = 1
      end if

   end function __MANGLE(count)

   ! =======================
   !  clear
   ! =======================
   recursive subroutine __MANGLE(clear)(this)
      class(__alt_set), intent(inout) :: this

      call this%items%clear()
      call this%parents%clear()
      call this%lefts%clear()
      call this%rights%clear()
      call this%heights%clear()

      this%root = UNINITIALIZED
      this%tsize = 0

      return
         
   end subroutine __MANGLE(clear)

   ! =======================
   !  insert_single()
   ! =======================
   recursive subroutine __MANGLE(insert_single)(this, value, unused, is_new, iter)
      class(__alt_set), target, intent(inout) :: this
      __T_declare_dummy__, intent(in) :: value
      type (KeywordEnforcer), optional :: unused
      logical, optional, intent(out) :: is_new
      type(__alt_set_iterator), optional, intent(out) :: iter

      integer(kind=GFTL_SIZE_KIND) :: new
      integer(kind=GFTL_SIZE_KIND) :: parent
      logical :: eq

#if (defined(__vector_debug) && defined(_DEBUG___))
      print*,__FILE__,__LINE__
#endif
      if (present(unused)) print*,shape(unused)
      
      if (__MANGLE(exists)(this%root)) then
#ifdef _multi
        parent = this%find_index(value, .true.)
#else
        parent = this%find_index(value, .false.)


        if (__MANGLE(exists)(parent)) then
           eq = __MANGLE(order_eq)(this%items%of(parent), value)
        else
           eq = .false.
        end if

        if (eq) then
           if (present(iter)) then
              iter%tree => this
              iter%current = parent
           else
              call this%items%set(parent, value)
           end if
           if (present(is_new)) then
              is_new = .false.
           end if
           return
        endif
#endif

        if (present(is_new)) then
           is_new = .true.
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
           call this%heights%push_back(1_GFTL_SIZE_KIND)
           call this%lefts%push_back(UNINITIALIZED)
           call this%rights%push_back(UNINITIALIZED)
           call this%parents%push_back(parent)
        else
           new = this%next_free
           this%next_free = this%parents%of(new)
           call this%items%set(new, value)
           call this%heights%set(new, 1_GFTL_SIZE_KIND)
           call this%lefts%set(new, UNINITIALIZED)
           call this%rights%set(new, UNINITIALIZED)
           call this%parents%set(new, parent)
        end if

        if (present(iter)) then
           iter%tree => this
           iter%current = new
        end if

#if (defined(__vector_debug) && defined(_DEBUG___))
      print*,__FILE__,__LINE__
#endif
           if (__MANGLE(v_lessThan)(value, this%items%of(parent))) then
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
           call this%heights%push_back(1_GFTL_SIZE_KIND)
           call this%lefts%push_back(UNINITIALIZED)
           call this%rights%push_back(UNINITIALIZED)
           call this%parents%push_back(UNINITIALIZED)
        else
           new = this%next_free
           this%next_free = this%parents%of(new)
           call this%items%set(new, value)
           call this%heights%set(new, 1_GFTL_SIZE_KIND)
           call this%lefts%set(new, UNINITIALIZED)
           call this%rights%set(new, UNINITIALIZED)
           call this%parents%set(new, UNINITIALIZED)
        end if
        this%root = new

        if (present(iter)) then
           iter%tree => this
           iter%current = this%root
        end if
        
        if (present(is_new)) then
           is_new = .true.
        end if

      endif

      this%tsize = this%tsize + 1

      return

      if (present(unused)) print*,shape(unused)

   end subroutine __MANGLE(insert_single)


#ifdef __T_listable__
   ! =======================
   !  insert_initializer_list()
   ! =======================
   recursive subroutine __MANGLE(insert_initializer_list)(this, values)
      class(__alt_set), intent(inout) :: this
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
      class(__alt_set), intent(inout) :: this
      type(__alt_set_iterator), intent(in) :: first
      type(__alt_set_iterator), intent(in) :: last

      type(__alt_set_iterator) :: iter

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
      class(__alt_set), target, intent(inout) :: this
      type(__alt_set_iterator), intent(in) :: hint
      __T_declare_dummy__, intent(in) :: value
      type (KeywordEnforcer), optional :: unused
      type(__alt_set_iterator), optional, intent(out) :: iter

      call this%insert(value, iter=iter)

      __UNUSED_DUMMY(unused)
      __UNUSED_DUMMY(hint)
   end subroutine __MANGLE(insert_single_with_hint)




   logical function __MANGLE(v_lessThan)(x, y) result(less)
      __T_declare_dummy__, intent(in) :: x
      __T_declare_dummy__, intent(in) :: y

#if defined(__alt_set_LT)
      less = __alt_set_LT__(x,y)
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

 end function __MANGLE(v_lessThan)






 ! =======================
 !  erase
 ! =======================
 function __MANGLE(erase_iter)(this, position) result(iter)
    type(__alt_set_iterator) :: iter
    class(__alt_set), target, intent(inout) :: this
    type(__alt_set_iterator), intent(in) :: position

    type (__alt_set_iterator) :: last

    last = position
    call last%next()
    iter = this%erase(position, last)

 end function __MANGLE(erase_iter)


 function __MANGLE(erase_value)(this, value) result(n)
    integer(kind=GFTL_SIZE_KIND) :: n
    class(__alt_set), target, intent(inout) :: this
    __T_declare_dummy__, intent(in) :: value

    type(__alt_set_iterator) :: iter

    iter = this%find(value)
    if (iter /= this%end()) then
       iter = this%erase(iter)
#ifdef __alt_set_multi
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
    type(__alt_set_iterator) :: next_iter
    class(__alt_set), intent(inout) :: this
    type(__alt_set_iterator), intent(in) :: first
    type(__alt_set_iterator), intent(in) :: last

    integer(kind=GFTL_SIZE_KIND) :: parent
    integer(kind=GFTL_SIZE_KIND) :: pos

    next_iter = first
    
    do while (next_iter /= last)
       
       pos = next_iter%current
       call next_iter%next()
       
       if (__MANGLE(exists)(this%rights%of(pos))) then
          call this%erase_nonleaf(pos, 1)
       else if (__MANGLE(exists)(this%lefts%of(pos))) then
          call this%erase_nonleaf(pos, 0)
       else
          parent = this%parents%of(pos)
          if (__MANGLE(exists)(parent)) then
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


    return
 end function __MANGLE(erase_range)

 ! =======================
 !  begin
 ! =======================
 function __MANGLE(begin)(this) result(begin)
    class(__alt_set), target, intent(in) :: this
    type(__alt_set_iterator) :: begin

    begin%tree => this
    call begin%next()

    return
 end function __MANGLE(begin)

 ! =======================
 !  end
 ! =======================
 function __MANGLE(end)(this) result(end_)
    class(__alt_set), target, intent(in) :: this
    type(__alt_set_iterator) :: end_

    end_%tree => this

    return
 end function __MANGLE(end)

 ! =======================
 !  ftn_begin
 ! =======================
 function __MANGLE(ftn_begin)(this) result(begin)
    class(__alt_set), target, intent(in) :: this
    type(__alt_set_iterator) :: begin

    begin%tree => this

    return
 end function __MANGLE(ftn_begin)

 ! =======================
 !  end
 ! =======================
 function __MANGLE(ftn_end)(this) result(end_)
    class(__alt_set), target, intent(in) :: this
    type(__alt_set_iterator) :: end_

    ! brute force implementation for now
    end_ = next(this%begin(), this%size()-1)

    return
 end function __MANGLE(ftn_end)

 ! =======================
 !  lower_bound
 ! =======================
 function __MANGLE(lower_bound)(this, value) result(lb)
    type(__alt_set_iterator) :: lb
    class(__alt_set), target, intent(in) :: this
    __T_declare_dummy__, intent(in) :: value

    lb%tree => this
    lb%current = this%find_index(value, .false.)

    if (__T_LT__(this%items%of(lb%current), value)) then
       if (lb /= this%end()) call lb%next()
    end if

    return
 end function __MANGLE(lower_bound)

 ! =======================
 !  upper_bound
 ! =======================
 function __MANGLE(upper_bound)(this, value) result(ub)
    type(__alt_set_iterator) :: ub
    class(__alt_set), target, intent(in) :: this
    __T_declare_dummy__, intent(in) :: value

    ub%tree => this
    ub%current = this%find_index(value, .false.)

    if (.not. (__T_LT__(value, this%items%of(ub%current)))) then
       if (ub /= this%end()) call ub%next()
    end if

    return
 end function __MANGLE(upper_bound)

 ! =======================
 !  find_index
 ! =======================
 function find_index(this, value, last) result(idx)
    integer(kind=GFTL_SIZE_KIND) :: idx
    class(__alt_set), target, intent(in) :: this
    __T_declare_dummy__, intent(in) :: value
    logical, intent(in) :: last

    integer(kind=GFTL_SIZE_KIND) :: child

      idx = this%root
      if (__MANGLE(exists)(idx)) then
         do
            if (.not. last .and. (                                           &
                 &         __MANGLE(order_eq)(this%items%of(idx),value))) return
            child=merge(this%lefts%of(idx), this%rights%of(idx),              &
                 &         __MANGLE(v_lessThan)(value, this%items%of(idx)))
            
            if (.not. __MANGLE(exists)(child)) return
            idx = child
         end do
      end if

      end function find_index

 ! =======================
 !  rebalance
 ! =======================
   subroutine __MANGLE(rebalance)(this, pos, once)
      class(__alt_set), intent(inout) :: this
      integer(GFTL_SIZE_KIND), intent(in) :: pos
      logical, intent(in) :: once

      integer(kind=GFTL_SIZE_KIND) :: curr, child

      integer :: hl, hr, chl, chr, side, childside
      logical :: unbalanced

      integer(kind=GFTL_SIZE_KIND), pointer :: pLeft, pRight

      curr = pos

      do while (__MANGLE(exists)(curr))
         hl = 0
         hr = 0
         pLeft => this%lefts%of(curr)
         if (__MANGLE(exists)(pLeft)) hl = this%heights%of(pLeft)
         pRight => this%rights%of(curr)
         if (__MANGLE(exists)(pRight)) hr = this%heights%of(pRight)

         unbalanced = (abs(hl-hr) > 1)

         if (unbalanced) then
            side = merge(__LEFT, __RIGHT, hl>hr)
            child = this%get_child(curr, side)

            chl=0
            chr=0

            pLeft => this%lefts%of(child)
            if (__MANGLE(exists)(pLeft)) chl = this%heights%of(pLeft)
            pRight => this%rights%of(child)
            if (__MANGLE(exists)(pRight)) chr = this%heights%of(pRight)

            if (chr /= chl) then
               childside=merge(0, 1, chl>chr)
               if (side/=childside)                                                &
     &            call this%rot(child,1-childside)
               call this%rot(curr, 1-side)
            endif
         endif
         call this%update_height(curr)


         if (unbalanced.and.once) exit

         curr = this%parents%of(curr)

      end do

      return
   end subroutine __MANGLE(rebalance)

   ! =======================
   !  erase_nonleaf
   ! =======================
   subroutine __MANGLE(erase_nonleaf)(this, pos, side)
      class(__alt_set), intent(inout) :: this
      integer(GFTL_SIZE_KIND), intent(inout) :: pos
      integer, intent(in) :: side

      integer(kind=GFTL_SIZE_KIND) :: parent, other, child0, child1,           &
     &      otherchild, otherparent

      parent = this%parents%of(pos)
      other = pos
      call this%advpos(other, side)

      if (side == 0) then
         child0 = this%lefts%of(pos)
         child1 = this%rights%of(pos)
         otherchild = this%lefts%of(other)
      else
         child0 = this%rights%of(pos)
         child1 = this%lefts%of(pos)
         otherchild = this%rights%of(other)
      end if
      otherparent = this%parents%of(other)
      call this%parents%set(other, parent)

      if (__MANGLE(exists)(parent)) then
        call this%set_parent_child(parent, pos, other)
      else
        this%root = other
      endif
      call this%set_child(other, 1-side, child1)
      if (__MANGLE(exists)(child1)) call this%parents%set(child1, other)
      if (other == child0) then
        call this%rebalance(other, .false.)
      else
        call this%set_child(other, side, child0)
        call this%parents%set(child0, other)
        call this%set_child(otherparent, 1-side, otherchild)
        if (__MANGLE(exists)(otherchild)) then
           call this%parents%set(otherchild, otherparent)
        end if
        call this%rebalance(otherparent, .false.)
      endif

      ! declare this space available
      call this%parents%set(pos, this%next_free)
      this%next_free = pos

      pos = UNINITIALIZED


      return
   end subroutine __MANGLE(erase_nonleaf)

   ! =======================
   !  advpos
   ! =======================
   recursive subroutine __MANGLE(advpos)(this, pos, dir)
      class(__alt_set), target, intent(in) :: this
      integer(GFTL_SIZE_KIND), intent(inout) :: pos
      integer, intent(in) :: dir   ! dir=1 forward, dir=0 backward

      integer(kind=GFTL_SIZE_KIND) :: prev
      integer(kind=GFTL_SIZE_KIND) :: child

      if (.not. __MANGLE(exists)(pos)) then
         if (.not. __MANGLE(exists)(this%root)) then
            return
         else
            pos = this%root
            do 
               child = this%get_child(pos, 1-dir)
               if (__MANGLE(exists)(child)) then
                  pos = child
               else
                  exit
               end if
            end do
         end if
      else
         child = this%get_child(pos, dir)
         if (__MANGLE(exists)(child)) then
            pos = child
            do 
               child = this%get_child(pos, 1-dir)
               if (__MANGLE(exists)(child)) then
                  pos = child
               else
                  exit
               end if
            end do
         else
            prev = pos
            pos = this%parents%of(pos)
            
            do while (__MANGLE(exists)(pos))
               child = this%get_child(pos, dir)
               if (child /= prev) exit
               prev = pos
               pos = this%parents%of(pos)
            end do
         endif
      end if

      return
   end subroutine __MANGLE(advpos)

   ! =======================
   !  rot
   ! =======================
   subroutine __MANGLE(rot)(this, pos, dir)
      class(__alt_set), intent(inout) :: this
      integer(kind=GFTL_SIZE_KIND), intent(inout) :: pos
      integer, intent(in) :: dir

      integer(kind=GFTL_SIZE_KIND) :: parent, child, grandchild

      parent = this%parents%of(pos)
      child = this%get_child(pos, 1-dir)

      if (__MANGLE(exists)(child)) then
         grandchild = this%get_child(child, dir)
      else
         grandchild = UNINITIALIZED
      end if

      if (__MANGLE(exists)(parent)) then
         call this%set_parent_child(parent, pos, child)
      else ! pos must be root; make the child root instead
         this%root = child
      endif

      ! 'child' is now my parent
      call this%parents%set(pos, child)

      ! 'grandchild' becomes now my child
      call this%set_child(pos, 1-dir, grandchild)

      ! fix up child
      if (__MANGLE(exists)(child)) then
         call this%parents%set(child, parent)
         call this%set_child(child, dir, pos)
         ! and fix up grandchild
         if (__MANGLE(exists)(grandchild)) then
            call this%parents%set(grandchild, pos)
         end if
      end if

      call this%update_height(pos)
      
      if (__MANGLE(exists)(child)) call this%update_height(child)

      return
   end subroutine __MANGLE(rot)

   logical function __MANGLE(value_compare)(this, x, y) result(value_compare)
      class(__alt_set), intent(in) :: this
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
      type(__alt_set), target, intent(in) :: a
      type(__alt_set), target, intent(in) :: b

      type (__alt_set_iterator) :: iter_a
      type (__alt_set_iterator) :: iter_b
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
      type(__alt_set), intent(in) :: a
      type(__alt_set), intent(in) :: b

      not_equal = .not. (a == b)

   end function __MANGLE(not_equal)

   ! ======================
   !  operator(<) less_than()
   ! ======================
   logical function __MANGLE(less_than)(a,b) result(lt)
      type(__alt_set), intent(in) :: a
      type(__alt_set), intent(in) :: b

      type (__alt_set_iterator) :: iter_a
      type (__alt_set_iterator) :: iter_b
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
      type(__alt_set), intent(in) :: a
      type(__alt_set), intent(in) :: b

      le = .not. (b < a)
      return
   end function __MANGLE(less_than_or_equal)

   ! ======================
   !  operator(>) - greater_than()
   ! ======================
   logical function __MANGLE(greater_than)(a,b) result(gt)
      type(__alt_set), intent(in) :: a
      type(__alt_set), intent(in) :: b

      gt = (b < a)
      return
   end function __MANGLE(greater_than)

   ! ======================
   !  operator(>=) - greater_than_or_equal() 
   ! ======================
   logical function __MANGLE(greater_than_or_equal)(a,b) result(ge)
      type(__alt_set), intent(in) :: a
      type(__alt_set), intent(in) :: b

      ge = .not. (a < b)
      return
   end function __MANGLE(greater_than_or_equal)

   ! =======================
   !   deep_copy (assignment)
   ! =======================
   recursive subroutine __MANGLE(deep_copy)(this, other)
      class(__alt_set), target, intent(out) :: this
      class(__alt_set), target, intent(in) :: other

      this%root = other%root
      this%tsize = other%tsize
      this%next_free = other%next_free

      this%items = other%items
      this%parents = other%parents
      this%lefts = other%lefts
      this%rights = other%rights
      this%heights = other%heights
      
   end subroutine __MANGLE(deep_copy)

#ifdef __T_listable__
   subroutine __MANGLE(copy_list)(this, il)
      class(__alt_set), intent(out) :: this
      __T_declare_dummy__, intent(in) :: il(:)

      call this%insert(il)

   end subroutine __MANGLE(copy_list)
#endif
   ! Suboptimal implementation
   recursive subroutine __MANGLE(merge)(this, source)
      class(__alt_set), intent(inout) :: this
      type(__alt_set), target, intent(inout) :: source

      type(__alt_set_iterator) :: iter

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
    class(__alt_set), intent(in) :: this
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
      class(__alt_set), target, intent(inout) :: this
      type(__alt_set), target, intent(inout) :: x

      integer(kind=GFTL_SIZE_KIND) :: tsize


      call this%items%swap(x%items)

      call this%parents%swap(x%parents)
      call this%lefts%swap(x%lefts)
      call this%rights%swap(x%rights)
      call this%heights%swap(x%heights)

      tsize = this%tsize
      this%tsize = x%tsize
      x%tsize = tsize

      tsize = this%root
      this%root = x%root
      x%root = tsize
      
      tsize = this%next_free
      this%next_free = x%next_free
      x%next_free = tsize
      
      return
   end subroutine __MANGLE(swap)





! =======================
!  set_child
! =======================
      subroutine __MANGLE(set_child)(this, pos, dir, child)
         class(__alt_set), intent(inout) :: this
         integer(kind=GFTL_SIZE_KIND), intent(in) :: pos
         integer, intent(in) :: dir
         integer(kind=GFTL_SIZE_KIND), intent(in) :: child

         select case (dir)
         case (__LEFT)
           if (child == 0) then
           end if
            call this%lefts%set(pos, child)
         case (__RIGHT)
            call this%rights%set(pos, child)
         end select

      end subroutine __MANGLE(set_child)

! =======================
!  set_parent_child
! =======================
      subroutine __MANGLE(set_parent_child)(this, parent, pos, other)
         class(__alt_set), intent(inout) :: this
         integer(kind=GFTL_SIZE_KIND), intent(in) :: parent
         integer(kind=GFTL_SIZE_KIND), intent(in) :: pos
         integer(kind=GFTL_SIZE_KIND), intent(in) :: other

         if (this%lefts%of(parent) == pos) then
            call this%set_child(parent, __LEFT, other)
         else
            call this%set_child(parent, __RIGHT, other)
         end if

      end subroutine __MANGLE(set_parent_child)

! =======================
!  child
! =======================
      function __MANGLE(get_child)(this, pos, dir) result(child)
         integer(kind=GFTL_SIZE_KIND) :: child
         class(__alt_set), intent(in) :: this
         integer(kind=GFTL_SIZE_KIND), intent(in) :: pos
         integer, intent(in) :: dir

         select case (dir)
         case (__LEFT)
            child = this%lefts%of(pos)
         case (__RIGHT)
            child = this%rights%of(pos)
         end select

      end function __MANGLE(get_child)


! =======================
!  update_height
! =======================
      subroutine __MANGLE(update_height)(this, pos) 
         class (__alt_set), intent(inout) :: this
         integer(kind=GFTL_SIZE_KIND), intent(in) :: pos

         integer(kind=GFTL_SIZE_KIND) :: idx
         integer(kind=GFTL_SIZE_KIND) :: new_height

         new_height = 0

         idx = this%lefts%of(pos)
         if (__MANGLE(exists)(idx)) then
            new_height = this%heights%of(idx)
         end if

         idx = this%rights%of(pos)
         if (__MANGLE(exists)(idx)) then
            new_height = max(new_height, this%heights%of(idx))
         end if
         
         new_height = new_height + 1
         call this%heights%set(pos, new_height)

      end subroutine __MANGLE(update_height)

! =======================
!  exists - a helper function
! =======================
      logical function __MANGLE(exists)(pos) result(exists)
         integer(kind=GFTL_SIZE_KIND), intent(in) :: pos
         exists = (pos /= UNINITIALIZED)
      end function __MANGLE(exists)

#include "alt_set/iterator_procedures.inc"

#undef __LEFT
#undef __RIGHT

   ! undef derived template parameters
#include "parameters/T/undef_derived_macros.inc"

   ! undef internal template parameters
#undef __guard
#include "parameters/T/undef_internal.inc"

#include "parameters/T/copy_alt_set_T_to_algorithm_T.inc"

#define __algorithm_guard __IDENTITY(__alt_set_guard)
#define __algorithm_container __alt_set
#define __algorithm_iterator __alt_set_iterator
#include "algorithms/procedures.inc"   
#undef __algorithm_container
#undef __algorithm_iterator
#undef __algorithm_T
#undef __algorithm_guard
#include "parameters/T/undef_algorithm_T.inc"
   

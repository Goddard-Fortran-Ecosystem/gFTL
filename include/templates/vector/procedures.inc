!--------------------------------------------------------------------
! Copyright © 2020 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

!---------------
! The sequence of includes below seems a bit of overkill in the
! case of Pair containers.  However, it becomes necessary when
! considering higher order containers that use lower order
! containers.  E.g., Set uses Vector and Map uses Pair.
!---------------

! define generic internal template parameters from pair parameters.
#include "templates/vector/copy_vector_T_to_T.inc"
#define __guard __vector_guard

! define derived generic template parameters from internal parameters.
#include "parameters/T/define_derived_macros.inc"

   ! =======================
   !  Structure constructors
   ! =======================
   function __MANGLE(new_vector_empty)() result(v)
      type (__vector) :: v
      logical, parameter :: flag = .false.

      if (flag) print*,shape(v) ! avoid warning about unused return value

      return
   end function __MANGLE(new_vector_empty)


   function __MANGLE(new_vector_fill_size_kind)(n, val) result(v)
      type (__vector) :: v
      integer(kind=__gftl_size_kind), intent(in) :: n
      __T_declare_dummy__, optional, intent(in) :: val

      call v%resize(n, val)

      return
   end function __MANGLE(new_vector_fill_size_kind)


   function __MANGLE(new_vector_fill_default)(n, val) result(v)
      type (__vector) :: v
      integer, intent(in) :: n
      __T_declare_dummy__, optional, intent(in) :: val

      call v%resize(n, val)

      return
   end function __MANGLE(new_vector_fill_default)


   function __MANGLE(new_vector_range)(first, last) result(v)
      type (__vector) :: v
      type (__vector_iterator), intent(in) :: first
      type (__vector_iterator), intent(in) :: last

      type (__vector_iterator) :: iter

      iter = first
      do while (iter /= last)
         call v%push_back(iter%get())
      end do

      return
   end function __MANGLE(new_vector_range)


   function __MANGLE(new_vector_copy)(x) result(v)
      type (__vector) :: v
      type (__vector) :: x

      integer :: i

      call v%resize(x%size())
      do i = 1, x%size()
         __T_COPY__(v%elements(i), x%elements(i))
      end do

      return
   end function __MANGLE(new_vector_copy)


#ifdef __T_listable__
   function __MANGLE(new_vector_initializer_list)(il) result(v)
      type (__vector) :: v
      __T_declare_dummy__, dimension(:), intent(in) :: il ! initializer list

      integer :: i

      call v%resize(size(il))
      do i = 1, size(il)
         __T_COPY__(v%elements(i)%item, il(i))
      end do

      return
   end function __MANGLE(new_vector_initializer_list)
#endif

   ! =======================
   !  size
   ! =======================
   pure function __MANGLE(size)(this) result(res)
      class(__vector), intent(in) :: this
      integer(kind=__gftl_size_kind) :: res

      res=this%vsize

      return
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
   !  capacity - size of internal arrays; >= size()
   ! =======================
   pure function __MANGLE(capacity)(this) result(capacity)
      integer(kind=__gftl_size_kind) :: capacity
      class (__vector), intent(in) :: this

      if (allocated(this%elements)) then
         capacity = size(this%elements)
      else
         capacity = 0
      end if

      return
   end function __MANGLE(capacity)


   ! =======================
   !  empty() - is container empty?
   ! =======================
   pure logical function __MANGLE(empty)(this) result(empty)
      class(__vector), intent(in) :: this

      empty = this%vsize==0

   end function __MANGLE(empty)


   ! =======================
   !  at() - return ith element with bounds check
   !
   !  Negative indices are relative to the last element of the array
   !  ala Python.
   ! =======================
   function __MANGLE(at_size_kind)(this, i, rc) result(res)
      class(__vector), target, intent(in) :: this
      integer(KIND=__gftl_size_kind), intent(in) :: i
      integer, intent(out) :: rc
      __T_declare_result__, pointer :: res

      integer :: idx
      idx = merge(i, this%vsize+i, i > 0)

      if ((idx <= 0).or.(idx > this%vsize)) then
         rc = OUT_OF_RANGE
      else
         rc = 0
         res => this%elements(idx)%item
      end if

      return
   end function __MANGLE(at_size_kind)


   function __MANGLE(at_default)(this, i, rc) result(res)
      class(__vector), target, intent(in) :: this
      integer, intent(in) :: i
      __T_declare_result__, pointer :: res
      integer, intent(out) :: rc

      integer :: idx

      ! Cannot directly invoke at_sizek_kind() because result
      ! is undefined for out-of-bound indices
      
      idx = merge(int(i,kind=__gftl_size_kind), this%vsize+i, i > 0)

      if ((idx <= 0).or.(idx > this%vsize)) then
         rc = OUT_OF_RANGE
      else
         rc = 0
         res => this%elements(idx)%item
      end if

      return
   end function __MANGLE(at_default)


   ! =======================
   !  of - return ith element without bounds checks
   ! =======================
   function __MANGLE(of_size_kind)(this, i) result(res)
      class(__vector), target, intent(in) :: this
      integer(kind=__gftl_size_kind), intent(in) :: i
      __T_declare_result__, pointer :: res

      res => this%elements(i)%item

      return
   end function __MANGLE(of_size_kind)

   function __MANGLE(of_default)(this, i) result(res)
      class(__vector), target, intent(in) :: this
      integer, intent(in) :: i
      __T_declare_result__, pointer :: res

      res => this%elements(i)%item

      return
   end function __MANGLE(of_default)


#if !defined(__T_allocatable__) && !(__T_rank__ > 0)

   ! =======================
   !  get_data
   ! =======================
   function __MANGLE(data)(this) result(res)
      class(__vector), target, intent(in) :: this
      __T_declare_component__, dimension(:), pointer :: res

      res => this%elements(:this%vsize)%item

      return
   end function __MANGLE(data)
#endif

   ! =======================
   !  back() - last element; undefined behavior for empty container
   ! =======================
   function __MANGLE(back)(this) result(res)
      class(__vector), target, intent(in) :: this
      __T_declare_result__, pointer :: res

      res => this%elements(this%vsize)%item

      return
   end function __MANGLE(back)

   ! =======================
   !  front() ! first element; undefined behavior for empty container
   ! =======================
   function __MANGLE(front)(this) result(res)
      class(__vector), target, intent(in) :: this
      __T_declare_result__, pointer :: res

      res => this%elements(1)%item

      return
   end function __MANGLE(front)


   ! =======================
   !  set() - modify contents of existing item with 
   !  Negative indices are treated analogously to Python
   !  lists.
   ! =======================
   subroutine __MANGLE(set_size_kind)(this, i, value)
      class(__vector), intent(inout) :: this
      integer(kind=__gftl_size_kind), intent(in) :: i
      __T_declare_dummy__, intent(in) :: value

      integer(kind=__gftl_size_kind) :: idx

      idx = merge(i, this%vsize+i, i > 0)

      __T_FREE__(this%elements(idx)%item)
      __T_COPY__(this%elements(idx)%item, value)

      return
   end subroutine __MANGLE(set_size_kind)


   subroutine __MANGLE(set_default)(this, i, value)
      class(__vector), intent(inout) :: this
      integer, intent(in) :: i
      __T_declare_dummy__, intent(in) :: value

      call this%set(int(i,kind=__gftl_size_kind), value)

   end subroutine __MANGLE(set_default)

   ! =======================
   !  reset()
   ! =======================
   subroutine __MANGLE(reset)(this)
      class(__vector), intent(inout) :: this

      if (allocated(this%elements)) then
         deallocate(this%elements)
      end if
      this%vsize=0
      return
   end subroutine __MANGLE(reset)


#if !defined(__T_allocatable__) && !(__T_rank__ > 0)
   ! =======================
   !  copyFromArray() assignment(=)
   ! =======================
   subroutine __MANGLE(copyFromArray)(this, array)
      class(__vector), intent(inout) :: this
      __T_declare_component__, intent(in) :: array(:)

      integer(kind=__gftl_size_kind) :: n

      n = size(array)

      call this%reserve(n)
      this%elements(1:n)%item = array(1:n)
      this%vsize=n

      return
   end subroutine __MANGLE(copyFromArray)
#endif


#ifdef __T_EQ__
   ! =======================
   !  equal
   ! =======================
   impure elemental function __MANGLE(equal)(lhs, rhs) result(equal)
      logical :: equal
      type(__vector), intent(in) :: lhs
      type(__vector), intent(in) :: rhs
      integer :: i

      ! Same size?
      equal = (lhs%vsize == rhs%vsize)

      if (equal) then ! Same elements?

         do i = 1, lhs%vsize
            if (.not. (__T_EQ__(lhs%elements(i)%item,rhs%elements(i)%item))) then
               equal = .false.
               return
            end if
         end do
         ! all elements equal
         equal = .true.
      endif

   end function __MANGLE(equal)

   ! =======================
   !  not_equal
   ! =======================
   impure elemental function __MANGLE(not_equal)(lhs, rhs) result (not_equal)
      logical :: not_equal
      type(__vector), intent(in) :: lhs
      type(__vector), intent(in) :: rhs

      not_equal = .not. (lhs == rhs)

      return
   end function __MANGLE(not_equal)

#endif




#ifdef __T_LT__
   ! =======================
   !  operator(<) - use lexicographical compare
   ! =======================
   impure elemental function __MANGLE(less_than)(lhs, rhs) result(less_than)
      logical :: less_than
      type (__vector), intent(in) :: lhs
      type (__vector), intent(in) :: rhs

      integer :: i, n
      __T_declare_result__, pointer :: lhs_i, rhs_i
      
      n = min(lhs%size(), rhs%size())
      do i = 1, n
         lhs_i => lhs%of(i)
         rhs_i => rhs%of(i)

         if (__T_LT__(lhs_i, rhs_i)) then
            less_than = .true.
            return
         else if (__T_LT__(rhs_i,lhs_i)) then
            less_than = .false.
            return
         endif
      end do

      less_than = (n < rhs%size())

      return
   end function __MANGLE(less_than)

   impure elemental function __MANGLE(greater_than_or_equal_to)(lhs, rhs) &
        & result(greater_than_or_equal_to)
      logical :: greater_than_or_equal_to
      type (__vector), intent(in) :: lhs
      type (__vector), intent(in) :: rhs

      greater_than_or_equal_to = .not. (lhs < rhs)

      return
   end function __MANGLE(greater_than_or_equal_to)


   impure elemental function __MANGLE(greater_than)(lhs, rhs) result(greater_than)
      logical :: greater_than
      type (__vector), intent(in) :: lhs
      type (__vector), intent(in) :: rhs

      greater_than = (rhs < lhs)

      return
   end function __MANGLE(greater_than)


   impure elemental function __MANGLE(less_than_or_equal_to)(lhs, rhs) &
        &  result(less_than_or_equal_to)
      logical :: less_than_or_equal_to
      type (__vector), intent(in) :: lhs
      type (__vector), intent(in) :: rhs

      less_than_or_equal_to = .not. (rhs < lhs)

      return
   end function __MANGLE(less_than_or_equal_to)


#endif



   ! =======================
   !  push_back()
   ! =======================
   subroutine __MANGLE(push_back)(this, value, unused, rc)
      class(__vector), intent(inout) :: this
      __T_declare_dummy__, intent(in) :: value
      type (KeywordEnforcer), optional, intent(in) :: unused
      integer, optional, intent(out) :: rc

      __UNUSED_DUMMY(unused)

      call this%grow_to(this%vsize+1)
      call this%resize(this%vsize+1, value, rc=rc)

      return

   end subroutine __MANGLE(push_back)


   ! =======================
   !  pop_back()
   ! =======================
   subroutine __MANGLE(pop_back)(this)
      class(__vector), intent(inout) :: this

      call this%downsize(this%vsize - 1)

      return
   end subroutine __MANGLE(pop_back)


   ! =======================
   !  insert
   ! =======================
   subroutine __MANGLE(insert_size_kind)(                                                 &
        &   this, index, value, unused, rc)
      class(__vector), target, intent(inout) :: this

      integer(kind=__gftl_size_kind), intent(in) :: index

      __T_declare_dummy__, intent(in) :: value
      type (KeywordEnforcer), optional, intent(in) :: unused
      integer, optional, intent(out) :: rc

      __T_declare_component__ :: temp
      integer(kind=__gftl_size_kind) :: i, n

      __UNUSED_DUMMY(unused)

      n = this%vsize
      if (index==n+1) then
         call this%push_back(value)
         ! Workaround for NAG -6221 - temp needs some status
         __T_COPY__(temp,value)
         return
      endif
      call this%grow_to(this%vsize+1)
      __T_COPY__(temp,this%elements(n)%item)
      call this%resize(n+1, temp, rc=rc)

      do i = n, index, -1
         __T_MOVE__(this%elements(index)%item,value)
      end do

      __T_COPY__(this%elements(index)%item,value)

      return
   end subroutine __MANGLE(insert_size_kind)

   subroutine __MANGLE(insert_default)(this, index, value)
      class(__vector), intent(inout) :: this
      integer, intent(in) :: index
      __T_declare_dummy__, intent(in) :: value

      call this%insert(int(index,kind=__gftl_size_kind), value)
   end subroutine __MANGLE(insert_default)


   ! =======================
   !  resize()
   ! =======================
   subroutine __MANGLE(resize_size_kind)(this, newsize, value, unused, rc)
      class(__vector), intent(inout) :: this
      integer(kind=__gftl_size_kind), intent(in) :: newsize
      __T_declare_dummy__, optional, intent(in) :: value
      type (KeywordEnforcer), optional, intent(in) :: unused
      integer, optional, intent(out) :: rc

      integer(kind=__gftl_size_kind) :: oldSize
      integer(kind=__gftl_size_kind) :: i
      integer :: status

      __UNUSED_DUMMY(unused)

      if (newSize == this%vsize) return
      if (newSize < 0) then
         if (present(rc)) rc = ILLEGAL_INPUT
         return
      end if
      oldSize=this%vsize

      call this%reserve(newSize, rc=status)
      if (status/=0) then
         if (present(rc)) rc=status
         return
      end if


      this%vsize = newSize

      do i = newSize + 1, oldSize
         __T_FREE__(this%elements(i)%item)
      end do

      if (present(value) .and. (newsize>oldsize)) then
         do i = oldSize + 1, newSize
            __T_COPY__(this%elements(i)%item, value)
         end do
      endif

      return
   end subroutine __MANGLE(resize_size_kind)

   subroutine __MANGLE(resize_default)(this, newsize, value, unused, rc)
      class(__vector), intent(inout) :: this
      integer, intent(in) :: newsize
      __T_declare_dummy__, optional, intent(in) :: value
      type (KeywordEnforcer), optional, intent(in) :: unused
      integer, optional, intent(out) :: rc

      __UNUSED_DUMMY(unused)

      call this%resize(int(newsize,kind=__gftl_size_kind), value, rc=rc)

   end subroutine __MANGLE(resize_default)


   ! =======================
   !  downsize()
   ! =======================
   subroutine __MANGLE(downsize)(this, newsize)
      class(__vector), intent(inout) :: this
      integer(kind=__gftl_size_kind), intent(in) :: newsize  ! assumes newsize<=size()
      integer(kind=__gftl_size_kind) :: i

      if (newsize<this%vsize) then
         do i=newsize+1, this%vsize
            __T_FREE__(this%elements(i)%item)
         end do
         this%vsize=newsize
      endif

      return
   end subroutine __MANGLE(downsize)


   ! =======================
   !  clear
   ! =======================
   subroutine __MANGLE(clear)(this)
      class(__vector), intent(inout) :: this

      call this%downsize(int(0,kind=__gftl_size_kind))

      return
   end subroutine __MANGLE(clear)


   ! =======================
   !  shrink_to_fit
   ! =======================

   subroutine __MANGLE(shrink_to_fit)(this)
      class(__vector), intent(inout) :: this

      if (this%vsize<this%capacity()) then
         call this%set_capacity(this%vsize)
      endif
      return
   end subroutine __MANGLE(shrink_to_fit)


   ! =======================
   !  erase_one
   ! =======================
   subroutine __MANGLE(erase_one)(this, position)
      class(__vector), target, intent(inout) :: this
      type (__vector_iterator), intent(in)  :: position

      call this%erase(position, position+1)

      return
   end subroutine __MANGLE(erase_one)


   ! =======================
   !  erase_range
   ! =======================
   subroutine __MANGLE(erase_range)(this, first, last)
      class(__vector), target, intent(inout) :: this
      type (__vector_iterator), intent(in)  :: first
      type (__vector_iterator), intent(in)  :: last

      integer :: i, delta

      delta=last%current_index-first%current_index
      do i=last%current_index, this%vsize
         associate(a => this%elements(i-delta),b =>this%elements(i))
           __T_MOVE__(a%item, b%item)
         end associate
      end do
      do i = this%vsize - delta + 1, last%current_index - 1
         __T_FREE__(this%elements(i)%item)
      end do
      this%vsize=this%vsize-delta

      return
   end subroutine __MANGLE(erase_range)

   ! =======================
   !  reserve()
   ! =======================
   subroutine __MANGLE(reserve_size_kind)(this, capacity, unused, rc)
      class(__vector), intent(inout) :: this
      integer(kind=__gftl_size_kind), intent(in) :: capacity
      class(KeywordEnforcer), optional, intent(in) :: unused
      integer, optional, intent(out) :: rc

      __UNUSED_DUMMY(unused)

      if (present(rc)) then
         if (capacity > this%max_size()) then
            rc = LENGTH_ERROR
            return
         else
            rc = 0
         end if
      end if

      if (capacity>this%capacity()) then
         call this%set_capacity(capacity)
      endif

      return
   end subroutine __MANGLE(reserve_size_kind)

   subroutine __MANGLE(reserve_default)(this, capacity, unused, rc)
      class(__vector), intent(inout) :: this
      integer, intent(in) :: capacity
      class(KeywordEnforcer), optional, intent(in) :: unused
      integer, optional, intent(out) :: rc

      __UNUSED_DUMMY(unused)

      if (present(rc)) then
         if (capacity > this%max_size()) then
            rc = LENGTH_ERROR
            return
         else
            rc = 0
         end if
      end if

      call this%reserve(int(capacity,kind=__gftl_size_kind))
      return
   end subroutine __MANGLE(reserve_default)


   ! =======================
   !  set_capacity()
   ! =======================
   subroutine __MANGLE(set_capacity)(this, capacity)
      class(__vector), target, intent(inout) :: this
      integer(kind=__gftl_size_kind), intent(in) :: capacity  ! capacity must be >=0
      type(__MANGLE(wrapper)), dimension(:), allocatable :: temp
      integer(kind=__gftl_size_kind) :: i

      if (capacity>0) then                     ! capacity>0
         if (.not.allocated(this%elements)) then   ! not allocated
            allocate(this%elements(capacity))
         else                                      ! allocated
            allocate(temp(capacity))
            do i=1, this%vsize
               __T_MOVE__(temp(i)%item, this%elements(i)%item)
            end do
            deallocate(this%elements)
            call move_alloc(temp, this%elements)
         endif
      else if (allocated(this%elements)) then  ! capacity==0
         ! Note: vsize must be 0 to reach this point.
         deallocate(this%elements)
      endif

      return
   end subroutine __MANGLE(set_capacity)

   ! =======================
   !  grow_to()
   ! =======================
   subroutine __MANGLE(grow_to)(this, capacity)
      class(__vector), intent(inout) :: this
      integer(kind=__gftl_size_kind), intent(in) :: capacity

      if (capacity>this%capacity()) then
         call this%set_capacity(max(2*this%vsize, capacity)) ! gives O(n) algorithm for growing vector with push.
      endif

      return
   end subroutine __MANGLE(grow_to)

   ! =======================
   !  swap
   ! =======================
   subroutine __MANGLE(swap)(this, x)
      class(__vector), target, intent(inout) :: this
      type(__vector), target, intent(inout) :: x

      type(__MANGLE(wrapper)), dimension(:), allocatable :: tmp
      integer :: tmpsize

      call move_alloc(this%elements, tmp)
      call move_alloc(x%elements, this%elements)
      call move_alloc(tmp, x%elements)
      tmpsize=this%vsize
      this%vsize=x%vsize
      x%vsize=tmpsize

      return
   end subroutine __MANGLE(swap)


#ifdef __T_EQ__
   ! =======================
   !  get_index() Return index of first item equal to value
   !  If not found, return 0.
   ! =======================
      function __MANGLE(get_index)(this, value) result(i)
         class(__vector), target, intent(in) :: this
         __T_declare_dummy__, intent(in) :: value

         integer(kind=__gftl_size_kind) :: i
         
         do i=1, this%vsize
            if (__T_EQ__(this%elements(i)%item, value)) return
         end do
         i=0

         return
      end function __MANGLE(get_index)
#endif      


   ! =======================
   !  begin() - create an iterator
   ! =======================
   function __MANGLE(begin)(this) result(iter)
      type (__vector_iterator) :: iter
      class (__vector), target, intent(in) :: this

      iter%current_index = 1

      if (allocated(this%elements)) then
         iter%elements => this%elements
      else
         iter%elements => null()
      end if

   end function __MANGLE(begin)


   ! =======================
   !  end()
   !  Construct  forward iterator, initially set to just
   !  after last element of vector.
   ! =======================
   function __MANGLE(end)(this) result(iter)
      class (__vector), target, intent(in) :: this
      type (__vector_iterator) :: iter

      iter%current_index = this%size() + 1 ! past the end
      if (allocated(this%elements)) then
         iter%elements => this%elements
      else
         iter%elements => null()
      end if

   end function __MANGLE(end)


   ! =======================
   !  rbegin() - create a reverse iterator
   ! =======================
   function __MANGLE(rbegin)(this) result(iter)
      type (__vector_riterator) :: iter
      class (__vector), target, intent(in) :: this

      iter%current_index = this%vsize
      if (allocated(this%elements)) then
         iter%elements => this%elements
      else
         iter%elements => null()
      end if

   end function __MANGLE(rbegin)


   ! =======================
   !  rend()
   !  Construct  reverse iterator, initially set to just
   !  before first element of vector
   ! =======================
   function __MANGLE(rend)(this) result(iter)
      class (__vector), target, intent(in) :: this
      type (__vector_riterator) :: iter

      iter%current_index = 0 ! before beginning
      if (allocated(this%elements)) then
         iter%elements => this%elements
      else
         iter%elements => null()
      end if

   end function __MANGLE(rend)


#include "templates/vector/iterator_procedures.inc"
#include "templates/vector/reverse_iterator_procedures.inc"

! undef derived template parameters
#include "parameters/T/undef_derived_macros.inc"

! undef internal template parameters
#undef __guard
#include "parameters/T/undef_internal.inc"



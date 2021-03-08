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
#include "parameters/T/copy_vector_T_to_internal_T.inc"
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


   function __MANGLE(new_vector_fill_size_kind)(n, value) result(v)
      type (__vector) :: v
      integer(kind=GFTL_SIZE_KIND), intent(in) :: n
      __T_declare_dummy__, optional, intent(in) :: value

      call v%resize(n, value=value)

      return
   end function __MANGLE(new_vector_fill_size_kind)


   function __MANGLE(new_vector_fill_default)(n, value) result(v)
      type (__vector) :: v
      integer, intent(in) :: n
      __T_declare_dummy__, optional, intent(in) :: value

      call v%resize(n, value=value)

      return
   end function __MANGLE(new_vector_fill_default)


   function __MANGLE(new_vector_range)(first, last) result(v)
      type (__vector) :: v
      type (__vector_iterator), intent(in) :: first
      type (__vector_iterator), intent(in) :: last

      type (__vector_iterator) :: iter

      iter = first
      do while (iter /= last)
         call v%push_back(iter%of())
      end do

      return
   end function __MANGLE(new_vector_range)


   function __MANGLE(new_vector_copy)(x) result(v)
      type (__vector) :: v
      type (__vector) :: x

      integer(kind=GFTL_SIZE_KIND) :: i

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

      integer(kind=GFTL_SIZE_KIND) :: i

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
      integer(kind=GFTL_SIZE_KIND) :: res

      res=this%vsize

      return
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
   !  capacity - size of internal arrays; >= size()
   ! =======================
   pure function __MANGLE(capacity)(this) result(capacity)
      integer(kind=GFTL_SIZE_KIND) :: capacity
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
      integer(KIND=GFTL_SIZE_KIND), intent(in) :: i
      integer, intent(out) :: rc
      __T_declare_result__, pointer :: res

      integer(kind=GFTL_SIZE_KIND) :: idx

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

      integer(kind=GFTL_SIZE_KIND) :: idx

      ! Cannot directly invoke at_size_kind() because result
      ! is undefined for out-of-bound indices
      
      idx = merge(int(i,kind=GFTL_SIZE_KIND), this%vsize+i, i > 0)

      if ((idx <= 0).or.(idx > this%vsize)) then
         rc = OUT_OF_RANGE
         res => null()
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
      integer(kind=GFTL_SIZE_KIND), intent(in) :: i
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


#if !defined(__T_allocatable__) && !(__T_rank > 0)
   ! =======================
   !  get_data
   ! =======================
   function __MANGLE(data)(this) result(res)
      class(__vector), target, intent(in) :: this
      __T_declare_component__, dimension(:), pointer :: res

      ! __T_rank__
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
      integer(kind=GFTL_SIZE_KIND), intent(in) :: i
      __T_declare_dummy__, intent(in) :: value

      integer(kind=GFTL_SIZE_KIND) :: idx

      idx = merge(i, this%vsize+i, i > 0)

      __T_FREE__(this%elements(idx)%item)
      __T_COPY__(this%elements(idx)%item, value)

      return
   end subroutine __MANGLE(set_size_kind)


   subroutine __MANGLE(set_default)(this, i, value)
      class(__vector), intent(inout) :: this
      integer, intent(in) :: i
      __T_declare_dummy__, intent(in) :: value

      call this%set(int(i,kind=GFTL_SIZE_KIND), value)

   end subroutine __MANGLE(set_default)


   ! =======================
   !  insert()
   ! =======================
   function __MANGLE(insert_one)(this, pos, value) result(iter)
      type(__vector_iterator) :: iter
      class(__vector), target, intent(inout) :: this
      type(__vector_iterator), intent(in) :: pos
      __T_declare_dummy__, intent(in) :: value

      iter = this%insert(pos, 1_GFTL_SIZE_KIND, value)

   end function __MANGLE(insert_one)

   function __MANGLE(insert_count_size_kind)(this, pos, count, value) result(iter)
      type(__vector_iterator) :: iter
      class(__vector), target, intent(inout) :: this
      type(__vector_iterator), intent(in) :: pos
      integer(kind=GFTL_SIZE_KIND), intent(in) :: count
      __T_declare_dummy__, intent(in) :: value

      integer(kind=GFTL_SIZE_KIND) :: i, j

      call this%grow_to(this%vsize + count)

      do i = this%vsize, pos%current_index, -1
         __T_MOVE__(this%elements(i+count)%item, this%elements(i)%item)
      end do
      do i = 1, count
         j = pos%current_index + i - 1
         __T_COPY__(this%elements(pos%current_index + i - 1)%item, value)
      end do

      this%vsize = this%vsize + count
      iter%elements => this%elements
      iter%current_index = pos%current_index
      
   end function __MANGLE(insert_count_size_kind)

#ifndef  __gftl_disable_index_kind_overload
   function __MANGLE(insert_count_default)(this, pos, count, value) result(iter)
      type(__vector_iterator) :: iter
      class(__vector), target, intent(inout) :: this
      type(__vector_iterator), intent(in) :: pos
      integer, intent(in) :: count
      __T_declare_dummy__, intent(in) :: value

      iter = this%insert(pos, int(count,kind=GFTL_SIZE_KIND), value)

   end function __MANGLE(insert_count_default)
#endif
   
   function __MANGLE(insert_range)(this, pos, first, last, value) result(iter)
      type(__vector_iterator) :: iter
      class(__vector), target, intent(inout) :: this
      type(__vector_iterator), intent(in) :: pos
      type(__vector_iterator), intent(in) :: first
      type(__vector_iterator), intent(in) :: last
      __T_declare_dummy__, intent(in) :: value
      
      integer(kind=GFTL_SIZE_KIND) :: i, n
      
      n = last - first

      call this%grow_to(this%vsize + n)

      do i = this%vsize, pos%current_index, -1
         __T_MOVE__(this%elements(i+n)%item, this%elements(i)%item)
      end do

      do i = 0, n-1
         __T_COPY__(this%elements(pos%current_index + i)%item, first%of(i))
      end do

      this%vsize = this%vsize + n
      iter%elements => this%elements
      iter%current_index = pos%current_index

   end function __MANGLE(insert_range)

#ifdef __T_listable__
   function __MANGLE(insert_list)(this, pos, values) result(iter)
      type(__vector_iterator) :: iter
      class(__vector), target, intent(inout) :: this
      type(__vector_iterator), intent(in) :: pos
      __T_declare_dummy__, intent(in) :: values(:)
      
      integer(kind=GFTL_SIZE_KIND) :: i, n

      n = size(values)
      call this%grow_to(this%vsize + n)

      do i = this%vsize, pos%current_index, -1
         __T_MOVE__(this%elements(i+n)%item, this%elements(i)%item)
      end do
      do i = 1, n
         __T_COPY__(this%elements(pos%current_index + i - 1)%item, values(i))
      end do

      this%vsize = this%vsize + n
      iter%elements => this%elements
      iter%current_index = pos%current_index
      
   end function __MANGLE(insert_list)
#endif

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

#if !defined(__T_allocatable__) && !(__T_rank > 0)
   ! =======================
   !  copyFromArray() assignment(=)
   ! =======================
   subroutine __MANGLE(copy_from_array)(this, array)
      class(__vector), intent(inout) :: this
      __T_declare_component__, intent(in) :: array(:)

      integer(kind=GFTL_SIZE_KIND) :: n

      n = size(array)

      call this%reserve(n)
      this%elements(1:n)%item = array(1:n)
      this%vsize=n

      return
   end subroutine __MANGLE(copy_from_array)
#endif


#if defined(__T_EQ__) || defined(__T_LT__)
   ! =======================
   !  equal
   ! =======================
   impure elemental function __MANGLE(equal)(lhs, rhs) result(equal)
      logical :: equal
      type(__vector), intent(in) :: lhs
      type(__vector), intent(in) :: rhs

      integer(kind=GFTL_SIZE_KIND) :: i


      ! Same size?
      equal = (lhs%vsize == rhs%vsize)

      if (equal) then ! Same elements?

         do i = 1, lhs%vsize
#ifdef __T_EQ__
#if __T_rank > 0 && !defined(__T_shape)
            if (any(shape(lhs%elements(i)%item) /= shape(rhs%elements(i)%item))) then
               equal = .false.
               return
            endif
#endif
            if (.not. (__T_EQ__(lhs%elements(i)%item,rhs%elements(i)%item))) then
               equal = .false.
               return
            end if
#else
! then must have a less-than operator
            if ((__T_LT__(lhs%elements(i)%item,rhs%elements(i)%item))) then
               equal = .false.
               return
            elseif ((__T_LT__(rhs%elements(i)%item,lhs%elements(i)%item))) then
               equal = .false.
               return
            end if
#endif
         end do

         ! all elements equal
         equal = .true. ! redundant
      endif

      return
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

      integer(kind=GFTL_SIZE_KIND) :: i, n

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
      call this%resize(this%vsize+1, value=value, rc=rc)

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
   !  resize()
   ! =======================
   subroutine __MANGLE(resize_size_kind)(this, count, unused, value, rc)
      class(__vector), intent(inout) :: this
      integer(kind=GFTL_SIZE_KIND), intent(in) :: count
      type (KeywordEnforcer), optional, intent(in) :: unused
      __T_declare_dummy__, optional, intent(in) :: value
      integer, optional, intent(out) :: rc

      integer(kind=GFTL_SIZE_KIND) :: oldSize
      integer(kind=GFTL_SIZE_KIND) :: i
      integer :: status

      __UNUSED_DUMMY(unused)

      if (count == this%vsize) then
         if (present(rc)) rc = 0
         return
      end if
      if (count < 0) then
         if (present(rc)) rc = ILLEGAL_INPUT
         return
      end if

      if (present(rc)) rc = 0

      oldSize = this%vsize

      call this%reserve(count, rc=status)
      if (status/=0) then
         if (present(rc)) rc=status
         return
      end if

      this%vsize = count
      do i = count + 1, oldSize
         __T_FREE__(this%elements(i)%item)
      end do

      if (count > oldsize) then
         if (present(value)) then
            do i = oldSize + 1, count
               __T_COPY__(this%elements(i)%item, value)
            end do
         else
            do i = oldSize + 1, count
               __T_COPY__(this%elements(i)%item, __T_default__)
            end do
         end if
      endif

      return
   end subroutine __MANGLE(resize_size_kind)

   subroutine __MANGLE(resize_default)(this, count, unused, value, rc)
      class(__vector), intent(inout) :: this
      integer, intent(in) :: count
      type (KeywordEnforcer), optional, intent(in) :: unused
      __T_declare_dummy__, optional, intent(in) :: value
      integer, optional, intent(out) :: rc

      __UNUSED_DUMMY(unused)

      call this%resize(int(count,kind=GFTL_SIZE_KIND), value=value, rc=rc)

   end subroutine __MANGLE(resize_default)


   ! =======================
   !  downsize()
   ! =======================
   subroutine __MANGLE(downsize)(this, newsize)
      class(__vector), intent(inout) :: this
      integer(kind=GFTL_SIZE_KIND), intent(in) :: newsize  ! assumes newsize<=size()
      integer(kind=GFTL_SIZE_KIND) :: i

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

      call this%downsize(0_GFTL_SIZE_KIND)

      return
   end subroutine __MANGLE(clear)


   ! =======================
   !  shrink_to_fit
   ! =======================

   subroutine __MANGLE(shrink_to_fit)(this)
      class(__vector), intent(inout) :: this

      if (this%vsize < this%capacity()) then
         call this%set_capacity(this%vsize)
      endif
      return
   end subroutine __MANGLE(shrink_to_fit)


   ! =======================
   !  erase_one
   ! =======================
   function __MANGLE(erase_one)(this, position) result(new_iter)
      type(__vector_iterator), target :: new_iter
      class(__vector), target, intent(inout) :: this
      type (__vector_iterator), intent(in)  :: position

      new_iter = this%erase(position, position+1)

      return
   end function __MANGLE(erase_one)


   ! =======================
   !  erase_range
   ! =======================
   function __MANGLE(erase_range)(this, first, last) result(new_iter)
      type(__vector_iterator), target :: new_iter
      class(__vector), target, intent(inout) :: this
      type (__vector_iterator), intent(in)  :: first
      type (__vector_iterator), intent(in)  :: last

      integer(kind=GFTL_SIZE_KIND) :: i, delta

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

      new_iter%elements => this%elements
      new_iter%current_index = first%current_index

      return
   end function __MANGLE(erase_range)

   ! =======================
   !  reserve()
   ! =======================
   subroutine __MANGLE(reserve_size_kind)(this, capacity, unused, rc)
      class(__vector), intent(inout) :: this
      integer(kind=GFTL_SIZE_KIND), intent(in) :: capacity
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

      call this%reserve(int(capacity,kind=GFTL_SIZE_KIND))
      return
   end subroutine __MANGLE(reserve_default)


   ! =======================
   !  set_capacity()
   ! =======================
   subroutine __MANGLE(set_capacity)(this, capacity)
      class(__vector), target, intent(inout) :: this
      integer(kind=GFTL_SIZE_KIND), intent(in) :: capacity  ! capacity must be >=0
      type(__MANGLE(wrapper)), dimension(:), allocatable :: temp
      integer(kind=GFTL_SIZE_KIND) :: i

      if (capacity>0) then                     ! capacity>0
         if (.not.allocated(this%elements)) then   ! not allocated
            allocate(this%elements(capacity))
         else                                      ! allocated
            allocate(temp(capacity))
            do i = 1, this%vsize
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
      integer(kind=GFTL_SIZE_KIND), intent(in) :: capacity

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
      integer(kind=GFTL_SIZE_KIND) :: tmpsize

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

         integer(kind=GFTL_SIZE_KIND) :: i
         
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

      iter%iterator = this%end()

   end function __MANGLE(rbegin)


   ! =======================
   !  rend()
   !  Construct  reverse iterator, initially set to just
   !  before first element of vector
   ! =======================
   function __MANGLE(rend)(this) result(iter)
      class (__vector), target, intent(in) :: this
      type (__vector_riterator) :: iter

      iter%iterator = this%begin()

   end function __MANGLE(rend)



   subroutine __MANGLE(write_formatted)(this, unit, iotype, v_list, iostat, iomsg)
    class(__vector), intent(in) :: this
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    iostat = 0

#ifdef __T_name__
    write(unit,'(a)') 'Vector<' // __T_declare_string__ // '>'
#else
    write(unit,'(a)') 'Vector<' // 'unknown' // '>'
#endif
    write(unit,'(a)') new_line('a')
    write(unit,'(4x,a10,1x,i0)') 'size: ',this%size()
   end subroutine __MANGLE(write_formatted)


#include "vector/iterator_procedures.inc"
#include "vector/reverse_iterator_procedures.inc"
! undef derived template parameters
#include "parameters/T/undef_derived_macros.inc"

! undef internal template parameters
#undef __guard
#include "parameters/T/undef_internal.inc"


#include "parameters/T/copy_vector_T_to_algorithm_T.inc"
#define __algorithm_guard __IDENTITY(__vector_guard)
#define __algorithm_container __vector
#define __algorithm_iterator __vector_iterator
#define __algorithm_riterator __vector_riterator
#include "algorithms/procedures.inc"   
#undef __algorithm_container
#undef __algorithm_iterator
#undef __algorithm_riterator
#undef __algorithm_T
#undef __algorithm_guard
#include "parameters/T/undef_algorithm_T.inc"





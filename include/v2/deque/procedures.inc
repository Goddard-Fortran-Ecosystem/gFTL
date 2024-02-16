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
! containers.  E.g., Set uses Deque and Map uses Pair.
!---------------

#include "parameters/T/copy_deque_T_to_internal_T.inc"
#define __guard __deque_guard

#if defined(__deque_iterator)
#    define __deque_iterator__ __deque_iterator
#else
#    define __deque_iterator__ DequeIterator
#endif

#if defined(__deque_riterator)
#    define __deque_riterator__ __deque_riterator
#else
#    define __deque_riterator__ DequeRIterator
#endif

#if defined(__deque_bucket_size)
#    define __deque_bucket_size__ __deque_bucket_size
#else
#    define __deque_bucket_size__ 1
#endif

! define derived generic template parameters from internal parameters.
#include "parameters/T/define_derived_macros.inc"


   subroutine __MANGLE(unused_procedure)()
      ! The following are an adhoc way to suppress compiler
      ! warnings about parameters not used in this template
      __UNUSED_DUMMY(BAD_ALLOC)
      __UNUSED_DUMMY(TYPE_HAS_NO_DEFAULT_VALUE)
      __UNUSED_DUMMY(NO_TYPE__)
   end subroutine __MANGLE(unused_procedure)


   ! =======================
   !  Structure constructors
   ! =======================
   function __MANGLE(new_deque_empty)() result(v)
      type (__deque) :: v
      logical, parameter :: flag = .false.

      if (flag) print*,shape(v) ! avoid warning about unused return value

      return
   end function __MANGLE(new_deque_empty)


   function __MANGLE(new_deque_fill_size_kind)(n, value) result(v)
      type (__deque) :: v
      integer(kind=GFTL_SIZE_KIND), intent(in) :: n
      __T_declare_dummy__, optional, intent(in) :: value

      call v%resize(n, value=value)

      return
   end function __MANGLE(new_deque_fill_size_kind)


   function __MANGLE(new_deque_fill_default)(n, value) result(v)
      type (__deque) :: v
      integer, intent(in) :: n
      __T_declare_dummy__, optional, intent(in) :: value

      call v%resize(n, value=value)

      return
   end function __MANGLE(new_deque_fill_default)


   function __MANGLE(new_deque_range)(first, last) result(v)
      type (__deque) :: v
      type (__deque_iterator__), intent(in) :: first
      type (__deque_iterator__), intent(in) :: last

      type (__deque_iterator__) :: iter

      iter = first
      do while (iter /= last)
         call v%push_back(iter%of())
      end do

      return
   end function __MANGLE(new_deque_range)


   function __MANGLE(new_deque_copy)(x) result(v)
      type (__deque) :: v
      type (__deque) :: x

      integer(kind=GFTL_SIZE_KIND) :: i

      call v%resize(x%size())
      do i = 1, x%size()
         v%buckets(i)%ptr%bucket_items = x%buckets(i)%ptr%bucket_items
      end do

      return
   end function __MANGLE(new_deque_copy)


!!$   function __MANGLE(new_deque_move)(x) result(v)
!!$      type (__deque) :: v
!!$      type (__deque) :: x
!!$
!!$      integer(kind=GFTL_SIZE_KIND) :: i
!!$
!!$      call v%resize(x%size())
!!$      do i = 1, x%size()
!!$         v%buckets(i)%ptr => x%buckets(i)%ptr
!!$      end do
!!$
!!$      ! Clear x without deallocating items
!!$      deallocate(x%buckets)
!!$
!!$      return
!!$   end function __MANGLE(new_deque_move)



   ! =======================
   !  size
   ! =======================
   pure function __MANGLE(size)(this) result(res)
      class(__deque), intent(in) :: this
      integer(kind=GFTL_SIZE_KIND) :: res

      res = this%size_

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
      class (__deque), intent(in) :: this

      if (allocated(this%buckets)) then
         capacity = size(this%buckets, kind=GFTL_SIZE_KIND)
      else
         capacity = 0
      end if

      return
   end function __MANGLE(capacity)


   ! =======================
   !  empty() - is container empty?
   ! =======================
   pure logical function __MANGLE(empty)(this) result(empty)
      class(__deque), intent(in) :: this

      empty = (this%size_==0)

   end function __MANGLE(empty)


   ! Hopefully this is inlined
   function __MANGLE(idx)(this, i) result(idx)
      integer(kind=GFTL_SIZE_KIND) :: idx
      class(__deque), intent(in) :: this
      integer(kind=GFTL_SIZE_KIND) :: i

      idx = 1 + modulo(this%front_ + (i-1) - 1, this%capacity())

   end function __MANGLE(idx)


   ! =======================
   !  at() - return ith element with bounds check
   ! =======================
   function __MANGLE(at_size_kind)(this, i, rc) result(res)
      class(__deque), target, intent(in) :: this
      integer(KIND=GFTL_SIZE_KIND), intent(in) :: i
      integer, intent(out) :: rc
      __T_declare_result__, pointer :: res

      if ((i <= 0) .or. (i > this%size_)) then
         rc = OUT_OF_RANGE
         res => null()
      else
         rc = SUCCESS
         res => this%buckets(this%idx(i))%ptr%bucket_items%item
      end if

      return
   end function __MANGLE(at_size_kind)


   function __MANGLE(at_default)(this, i, rc) result(res)
      class(__deque), target, intent(in) :: this
      integer, intent(in) :: i
      __T_declare_result__, pointer :: res
      integer, intent(out) :: rc

      res => this%at(int(i,GFTL_SIZE_KIND),rc)

      return
   end function __MANGLE(at_default)


   ! =======================
   !  of - return ith element without bounds checks
   ! =======================
   function __MANGLE(of_size_kind)(this, i) result(res)
      class(__deque), target, intent(in) :: this
      integer(kind=GFTL_SIZE_KIND), intent(in) :: i
      __T_declare_result__, pointer :: res


      res => this%buckets(this%idx(i))%ptr%bucket_items%item

      return
   end function __MANGLE(of_size_kind)

   function __MANGLE(of_default)(this, i) result(res)
      class(__deque), target, intent(in) :: this
      integer, intent(in) :: i
      __T_declare_result__, pointer :: res

      res => this%at(int(i,GFTL_SIZE_KIND))

      return
   end function __MANGLE(of_default)


   ! =======================
   !  back() - last element; undefined behavior for empty container
   ! =======================
   function __MANGLE(back)(this) result(res)
      class(__deque), target, intent(in) :: this
      __T_declare_result__, pointer :: res

      res => this%buckets(this%back_)%ptr%bucket_items%item

      return
   end function __MANGLE(back)

   ! =======================
   !  front() ! first element; undefined behavior for empty container
   ! =======================
   function __MANGLE(front)(this) result(res)
      class(__deque), target, intent(in) :: this
      __T_declare_result__, pointer :: res

      res => this%buckets(this%front_)%ptr%bucket_items%item

      return
   end function __MANGLE(front)


   ! =======================
   !  set() - modify contents of existing item with 
   ! =======================
   subroutine __MANGLE(set_size_kind)(this, i, value)
      class(__deque), intent(inout) :: this
      integer(kind=GFTL_SIZE_KIND), intent(in) :: i
      __T_declare_dummy__, intent(in) :: value

      integer(kind=GFTL_SIZE_KIND) :: ii

      ii = this%idx(i)
      __T_FREE__(this%buckets(ii)%ptr%bucket_items%item)
      __T_COPY__(this%buckets(ii)%ptr%bucket_items%item, value)


      return
   end subroutine __MANGLE(set_size_kind)


   subroutine __MANGLE(set_default)(this, i, value)
      class(__deque), intent(inout) :: this
      integer, intent(in) :: i
      __T_declare_dummy__, intent(in) :: value

      call this%set(int(i,kind=GFTL_SIZE_KIND), value)

   end subroutine __MANGLE(set_default)

   ! =======================
   !  reset()
   ! =======================
   subroutine __MANGLE(reset)(this)
      class(__deque), intent(inout) :: this

      call this%clear()
      if (allocated(this%buckets)) then
         deallocate(this%buckets)
      end if
      this%front_ = -1
      this%back_ = 0

      return
   end subroutine __MANGLE(reset)


#if defined(__T_EQ__) || defined(__T_LT__)
   ! =======================
   !  equal
   ! =======================
   impure elemental function __MANGLE(equal)(lhs, rhs) result(equal)
      logical :: equal
      type(__deque), intent(in) :: lhs
      type(__deque), intent(in) :: rhs

      integer(kind=GFTL_SIZE_KIND) :: i, ii

      ! Same size?
      equal = (lhs%front_ == rhs%front_) .and. (lhs%back_ == rhs%back_)

      if (equal) then ! Same elements?
         
         do i = 1, lhs%size_
            ii = lhs%idx(i)

            associate ( &
                 item_l => lhs%buckets(ii)%ptr%bucket_items%item, &
                 item_r => rhs%buckets(ii)%ptr%bucket_items%item)
#ifdef __T_EQ__
#if __T_rank > 0 && !defined(__T_shape)
              if (any(shape(item_l) /= shape(item_r))) then
               equal = .false.
               return
            endif
#endif
            if (.not. (__T_EQ__(item_l, item_r))) then
               equal = .false.
               return
            end if
#else
! then must have a less-than operator
            if ((__T_LT__(item_l,item_r))) then
               equal = .false.
               return
            elseif ((__T_LT__(item_r,item_l))) then
               equal = .false.
               return
            end if
#endif
            end associate
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
      type(__deque), intent(in) :: lhs
      type(__deque), intent(in) :: rhs

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
      type (__deque), intent(in) :: lhs
      type (__deque), intent(in) :: rhs

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
      type (__deque), intent(in) :: lhs
      type (__deque), intent(in) :: rhs

      greater_than_or_equal_to = .not. (lhs < rhs)

      return
   end function __MANGLE(greater_than_or_equal_to)


   impure elemental function __MANGLE(greater_than)(lhs, rhs) result(greater_than)
      logical :: greater_than
      type (__deque), intent(in) :: lhs
      type (__deque), intent(in) :: rhs

      greater_than = (rhs < lhs)

      return
   end function __MANGLE(greater_than)


   impure elemental function __MANGLE(less_than_or_equal_to)(lhs, rhs) &
        &  result(less_than_or_equal_to)
      logical :: less_than_or_equal_to
      type (__deque), intent(in) :: lhs
      type (__deque), intent(in) :: rhs

      less_than_or_equal_to = .not. (rhs < lhs)

      return
   end function __MANGLE(less_than_or_equal_to)


#endif



   ! =======================
   !  push_back()
   ! =======================
   subroutine __MANGLE(push_back)(this, value, unused, rc)
      class(__deque), intent(inout) :: this
      __T_declare_dummy__, intent(in) :: value
      type (KeywordEnforcer), optional, intent(in) :: unused
      integer, optional, intent(out) :: rc

      __UNUSED_DUMMY(unused)

      call this%grow_to(this%size_+1)

      if (this%back_ == this%capacity()) then
         this%back_ = 1
      else
         this%back_ = this%back_ + 1
      end if

      ! Special case: first element also changes front
      if (this%size_ == 0) this%front_ = this%back_
      call this%resize(this%size_+1, value=value, rc=rc)

      return

   end subroutine __MANGLE(push_back)


   ! =======================
   !  pop_back()
   ! =======================
   subroutine __MANGLE(pop_back)(this)
      class(__deque), intent(inout) :: this

      integer(kind=GFTL_SIZE_KIND) :: old_back

      old_back = this%back_

      if (this%back_ == 1) then
         this%back_ = this%capacity()
      else
         this%back_ = this%back_ - 1
      end if

      call this%downsize(old_back, old_back)


      return
   end subroutine __MANGLE(pop_back)


   ! =======================
   !  push_front()
   ! =======================
   subroutine __MANGLE(push_front)(this, value, unused, rc)
      class(__deque), intent(inout) :: this
      __T_declare_dummy__, intent(in) :: value
      type (KeywordEnforcer), optional, intent(in) :: unused
      integer, optional, intent(out) :: rc

      __UNUSED_DUMMY(unused)

      if (this%front_ == 1) then
         this%front_ = this%size_
      else
         this%front_ = this%front_ - 1
      end if

      call this%grow_to(this%size_+1)

      ! Special case: first element also changes back
      if (this%size_ == 0) this%back_ = this%front_

      call this%resize(this%size_+1, value=value, rc=rc)

      return

   end subroutine __MANGLE(push_front)


   ! =======================
   !  pop_front()
   ! =======================
   subroutine __MANGLE(pop_front)(this)
      class(__deque), intent(inout) :: this

      integer(kind=GFTL_SIZE_KIND) :: old_front
      
      old_front = this%front_
      if (this%front_ == this%capacity()) then
         this%front_ = 1
      else
         this%front_ = this%front_ + 1
      end if
      
      call this%downsize(old_front, old_front)

      return
   end subroutine __MANGLE(pop_front)



   ! =======================
   !  resize()
   ! =======================
   subroutine __MANGLE(resize_size_kind)(this, count, unused, value, rc)
      class(__deque), intent(inout) :: this
      integer(kind=GFTL_SIZE_KIND), intent(in) :: count
      type (KeywordEnforcer), optional, intent(in) :: unused
      __T_declare_dummy__, optional, intent(in) :: value
      integer, optional, intent(out) :: rc

      integer(kind=GFTL_SIZE_KIND) :: old_size
      integer :: status
#if defined(__T_FREE__)
      integer(kind=GFTL_SIZE_KIND) :: i, ii
#endif

      if (count == this%size_) then
         if (present(rc)) rc = SUCCESS
         return
      end if
      if (count < 0) then
         if (present(rc)) rc = ILLEGAL_INPUT
         return
      end if

      if (present(rc)) rc = SUCCESS

      old_size = this%size_

      call this%reserve(count, rc=status)
      if (status/=0) then
         if (present(rc)) rc=status
         return
      end if

      this%size_ = count
#if defined(__T_FREE__)
      do i = count + 1, old_size
         ii = this%idx(i)
         __T_FREE__(this%buckets(ii)%ptr%bucket_items%item)
      end do

      if (count > old_size) then
         if (present(value)) then
            do i = old_size + 1, count
               ii = this%idx(i)
               __T_COPY__(this%buckets(ii)%ptr%bucket_items%item, value)
            end do
         else
            do i = old_size + 1, count
               ii = this%idx(i)
               __T_COPY__(this%buckets(ii)%ptr%bucket_items%item, __T_default__)
            end do
         end if
      endif
#endif

      return
      __UNUSED_DUMMY(unused)
   end subroutine __MANGLE(resize_size_kind)

   subroutine __MANGLE(resize_default)(this, count, unused, value, rc)
      class(__deque), intent(inout) :: this
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
   subroutine __MANGLE(downsize)(this, from, to)
      class(__deque), intent(inout) :: this
      integer(kind=GFTL_SIZE_KIND), intent(in) :: from, to  ! assumes newsize<=size()

#if defined(__T_FREE__)
      integer(kind=GFTL_SIZE_KIND) :: i, ii
      if (from <= to) then

         do i = from, to - 1
            ii = this%idx(i)
            __T_FREE__(this%buckets(ii)%ptr%bucket_items%item)
         end do

      else

         do i = from, this%capacity()
            ii = this%idx(i)
            __T_FREE__(this%buckets(ii)%ptr%bucket_items%item)
         end do
         do i = 1, to
            ii = this%idx(i)
            __T_FREE__(this%buckets(ii)%ptr%bucket_items%item)
         end do

      endif
      __UNUSED_DUMMY(ii)
#endif

      this%size_ = this%size_ - modulo(from - to, this%capacity())  - 1

      return
   end subroutine __MANGLE(downsize)


   ! =======================
   !  clear
   ! =======================
   subroutine __MANGLE(clear)(this)
      class(__deque), intent(inout) :: this

      call this%downsize(this%front_, this%back_)
      this%front_ = -1
      this%back_ = 0

      return
   end subroutine __MANGLE(clear)


   ! =======================
   !  shrink_to_fit
   ! =======================

   subroutine __MANGLE(shrink_to_fit)(this)
      class(__deque), intent(inout) :: this

      if (this%size_ < this%capacity()) then
         call this%set_capacity(this%size_)
      endif
      return
   end subroutine __MANGLE(shrink_to_fit)


   ! =======================
   !  erase_one
   ! =======================
   function __MANGLE(erase_one)(this, position) result(new_iter)
      type(__deque_iterator__), target :: new_iter
      class(__deque), target, intent(inout) :: this
      type (__deque_iterator__), intent(in)  :: position

      new_iter = this%erase(position, position+1)

      return
   end function __MANGLE(erase_one)


   ! =======================
   !  erase_range
   ! =======================
   function __MANGLE(erase_range)(this, first, last) result(new_iter)
      type(__deque_iterator__), target :: new_iter
      class(__deque), target, intent(inout) :: this
      type (__deque_iterator__), intent(in)  :: first
      type (__deque_iterator__), intent(in)  :: last

      integer(kind=GFTL_SIZE_KIND) :: i, ii, delta

      delta=last%current_index-first%current_index
      if (delta == 0) then
         new_iter = last
         return
      end if
      do i=last%current_index, this%size_
         ii = this%idx(i)
         associate( &
              a => this%buckets(ii-delta)%ptr%bucket_items, &
              b => this%buckets(ii)%ptr%bucket_items)
              __T_MOVE__(a%item, b%item)
         end associate
      end do
      do i = this%size_ - delta + 1, last%current_index - 1
         ii = this%idx(i)
         __T_FREE__(this%buckets(ii)%ptr%bucket_items%item)
      end do
      this%size_=this%size_-delta

      ! back may wrap around
      this%back_ = 1 + modulo(this%back_ - delta - 1, this%size_)

      new_iter%deque => this
      new_iter%current_index = first%current_index

      return
   end function __MANGLE(erase_range)

   ! =======================
   !  reserve()
   ! =======================
   subroutine __MANGLE(reserve_size_kind)(this, capacity, unused, rc)
      class(__deque), intent(inout) :: this
      integer(kind=GFTL_SIZE_KIND), intent(in) :: capacity
      class(KeywordEnforcer), optional, intent(in) :: unused
      integer, optional, intent(out) :: rc

      __UNUSED_DUMMY(unused)

      if (present(rc)) then
         if (capacity > this%max_size()) then
            rc = LENGTH_ERROR
            return
         else
            rc = SUCCESS
         end if
      end if

      if (capacity>this%capacity()) then
         call this%set_capacity(capacity)
      endif

      return
   end subroutine __MANGLE(reserve_size_kind)

   subroutine __MANGLE(reserve_default)(this, capacity, unused, rc)
      class(__deque), intent(inout) :: this
      integer, intent(in) :: capacity
      class(KeywordEnforcer), optional, intent(in) :: unused
      integer, optional, intent(out) :: rc

      __UNUSED_DUMMY(unused)

      if (present(rc)) then
         if (capacity > this%max_size()) then
            rc = LENGTH_ERROR
            return
         else
            rc = SUCCESS
         end if
      end if

      call this%reserve(int(capacity,kind=GFTL_SIZE_KIND))
      return
   end subroutine __MANGLE(reserve_default)


   ! =======================
   !  set_capacity()
   ! =======================
   subroutine __MANGLE(set_capacity)(this, capacity)
      class(__deque), target, intent(inout) :: this
      integer(kind=GFTL_SIZE_KIND), intent(in) :: capacity  ! capacity must be >=0
      type(__MANGLE(bucket_ptr)), dimension(:), allocatable :: temp

      integer(kind=GFTL_SIZE_KIND) :: i, ii
      integer(kind=GFTL_SIZE_KIND) :: old_capacity

      if (capacity > 0) then                     ! capacity>0
         if (.not.allocated(this%buckets)) then   ! not allocated
            old_capacity = 0
            allocate(this%buckets(capacity))
         else                                      ! allocated
            old_capacity = size(this%buckets,kind=GFTL_SIZE_KIND)
            allocate(temp(capacity))
            do i = 1, min(old_capacity, capacity)
               ii = this%idx(i)
               ! Rebase front to start of array
               temp(i)%ptr => this%buckets(ii)%ptr
            end do
            ! Deallocations
            ! Free up excess buckets, if any
            do i= min(old_capacity,capacity)+1, old_capacity
               ii = this%idx(i)
               deallocate(this%buckets(ii)%ptr)
            end do
            deallocate(this%buckets)
            call move_alloc(temp, this%buckets)
            if (this%size_ > 0) then ! shift the markers
               this%front_ = 1
               this%back_ = this%size_
            end if
         endif
         ! Must still allocate any new buckets
         do i = old_capacity + 1, capacity
            ii = this%idx(i)
            allocate(this%buckets(ii)%ptr)
         end do
      else if (allocated(this%buckets)) then  ! capacity==0
         ! Note: size_ must be 0 to reach this point.

         do i = 1, old_capacity
            deallocate(this%buckets(i)%ptr)
         end do
         deallocate(this%buckets)
         
         this%front_ = -1
         this%back_ = 0
      endif

      return
   end subroutine __MANGLE(set_capacity)

   ! =======================
   !  grow_to()
   ! =======================
   subroutine __MANGLE(grow_to)(this, capacity)
      class(__deque), intent(inout) :: this
      integer(kind=GFTL_SIZE_KIND), intent(in) :: capacity

      if (capacity>this%capacity()) then
         call this%set_capacity(max(2*this%size_, capacity)) ! gives O(n) algorithm for growing deque with push.
      endif

      return
   end subroutine __MANGLE(grow_to)

   ! =======================
   !  swap
   ! =======================
   subroutine __MANGLE(swap)(this, x)
      class(__deque), target, intent(inout) :: this
      type(__deque), target, intent(inout) :: x

      type(__MANGLE(bucket_ptr)), dimension(:), allocatable :: tmp
      integer(kind=GFTL_SIZE_KIND) :: i_tmp

      call move_alloc(this%buckets, tmp)
      call move_alloc(x%buckets, this%buckets)
      call move_alloc(tmp, x%buckets)

      i_tmp = this%size_
      this%size_ = x%size_
      x%size_ = i_tmp

      i_tmp = this%front_
      this%front_ = x%front_
      x%front_ = i_tmp

      i_tmp = this%back_
      this%back_ = x%back_
      x%back_ = i_tmp
      
      return
   end subroutine __MANGLE(swap)


#ifdef __T_EQ__
   ! =======================
   !  get_index() Return index of first item equal to value
   !  If not found, return 0.
   ! =======================
      function __MANGLE(get_index)(this, value) result(i)
         class(__deque), target, intent(in) :: this
         __T_declare_dummy__, intent(in) :: value

         integer(kind=GFTL_SIZE_KIND) :: i, ii
         
         do i=1, this%size_
            ii = this%idx(i)
            if (__T_EQ__(this%buckets(ii)%ptr%bucket_items%item, value)) return
         end do
         i=0

         return
      end function __MANGLE(get_index)
#endif      


   ! =======================
   !  begin() - create an iterator
   ! =======================
   function __MANGLE(begin)(this) result(iter)
      type (__deque_iterator__) :: iter
      class (__deque), target, intent(in) :: this

      iter%current_index = 1
      iter%deque => this
      
   end function __MANGLE(begin)


   ! =======================
   !  end()
   !  Construct  forward iterator, initially set to just
   !  after last element of deque.
   ! =======================
   function __MANGLE(end)(this) result(iter)
      class (__deque), target, intent(in) :: this
      type (__deque_iterator__) :: iter

      iter%current_index = this%size() + 1 ! past the end
      iter%deque => this

   end function __MANGLE(end)


   ! =======================
   !  rbegin() - create a reverse iterator
   ! =======================
   function __MANGLE(rbegin)(this) result(iter)
      type (__deque_riterator__) :: iter
      class (__deque), target, intent(in) :: this

      iter%current_index = this%size_
      iter%deque => this

   end function __MANGLE(rbegin)


   ! =======================
   !  rend()
   !  Construct  reverse iterator, initially set to just
   !  before first element of deque
   ! =======================
   function __MANGLE(rend)(this) result(iter)
      class (__deque), target, intent(in) :: this
      type (__deque_riterator__) :: iter

      iter%current_index = 0 ! before beginning
      iter%deque => this

   end function __MANGLE(rend)


   subroutine __MANGLE(write_formatted)(this, unit, iotype, v_list, iostat, iomsg)
    class(__deque), intent(in) :: this
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    iostat = 0

#ifdef __T_name__
    write(unit,'(a)') 'Deque<' // __T_declare_string__ // '>'
#else
    write(unit,'(a)') 'Deque<' // 'unknown' // '>'
#endif
    write(unit,'(a)') new_line('a')
    write(unit,'(4x,a10,1x,i0)') 'size: ',this%size()

    __UNUSED_DUMMY(iomsg)
    __UNUSED_DUMMY(iotype)
    __UNUSED_DUMMY(v_list)
   end subroutine __MANGLE(write_formatted)


   function __MANGLE(bucket_index)(this, i) result(bucket_index)
      integer(kind=GFTL_SIZE_KIND) :: bucket_index
      class(__deque), intent(in) :: this
      integer(kind=GFTL_SIZE_KIND), intent(in) :: i

      bucket_index = 1 + (i-1)/__deque_bucket_size__

      __UNUSED_DUMMY(this)
   end function __MANGLE(bucket_index)

   function __MANGLE(bucket_offset)(this, i) result(bucket_offset)
      integer(kind=GFTL_SIZE_KIND) :: bucket_offset
      class(__deque), intent(in) :: this
      integer(kind=GFTL_SIZE_KIND), intent(in) :: i

      bucket_offset = 1 + modulo(i-1, int(__deque_bucket_size__,kind=GFTL_SIZE_KIND))

      __UNUSED_DUMMY(this)
   end function __MANGLE(bucket_offset)

#include "deque/iterator_procedures.inc"
#include "deque/reverse_iterator_procedures.inc"

#if defined(__deque_iterator__)
#    undef __deque_iterator__
#endif

#if defined(__deque_riterator__)
#    undef __deque_riterator__
#endif

#if defined(__deque_bucket_size__)
#    undef __deque_bucket_size__
#endif

! undef derived template parameters
#include "parameters/T/undef_derived_macros.inc"

! undef internal template parameters
#undef __guard
#include "parameters/T/undef_internal.inc"



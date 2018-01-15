!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

! Main container file must define
!   __vector
!   __iterator
!   __riterator
!   __container_prefix

#include "templates/all_template_macros.inc"


#ifdef __type_wrapped
#  define __declare_element_type type(__PROC(Wrapper))
#  define __GET(x) x%item
#else
#  define __declare_element_type __type_declare_type
#  define __GET(x) x
! hello __declare_element_type __PROC(x)
#endif


      function __PROC(new_empty)() result(v)
         type (__vector) :: v
         logical, parameter :: flag = .false.
         if (flag) print*,shape(v) ! avoid warning about unused return value
         return
      end function __PROC(new_empty)
      
      
      ! =======================
      !  size
      ! =======================
      pure function __PROC(size)(this) result(res)
         class(__vector), intent(in) :: this
         integer(kind=SIZE_KIND) :: res
         
         res=this%vsize
         return
      end function __PROC(size)
      
      ! =======================
      !  capacity
      ! =======================
      pure function __PROC(capacity)(this) result(capacity)
         integer(kind=SIZE_KIND) :: capacity
         class (__vector), intent(in) :: this
         
         if (allocated(this%elements)) then
            capacity = size(this%elements)
         else
            capacity = 0
         end if
         
      end function __PROC(capacity)
      
      
      ! =======================
      !  empty
      ! =======================
      pure logical function __PROC(empty)(this) result(empty)
         class(__vector), intent(in) :: this
         
         empty = this%vsize==0
         
      end function __PROC(empty)
      
      
      ! =======================
      !  at
      ! =======================
      function __PROC(at_size_kind)(this, i, unused, rc) result(res)
         class(__vector), target, intent(in) :: this
         integer(KIND=SIZE_KIND), intent(in) :: i
         type (KeywordEnforcer), optional, intent(in) :: unused
         integer, optional, intent(out) :: rc
         __type_declare_result, pointer :: res

         _UNUSED_DUMMY(unused)

         if ((i<=0).or.(i>this%vsize)) then
            if (present(rc)) rc = OUT_OF_RANGE
            return
         end if

         res=>__GET(this%elements(i))
         return
      end function __PROC(at_size_kind)

#ifdef SUPPORT_FOR_INT64
      function __PROC(at_32)(this, i, unused, rc) result(res)
         class(__vector), target, intent(in) :: this
         integer, intent(in) :: i
         __type_declare_result, pointer :: res
         type (KeywordEnforcer), optional, intent(in) :: unused
         integer, optional, intent(out) :: rc

         _UNUSED_DUMMY(unused)

         res => this%at_size_kind(int(i,kind=SIZE_KIND),rc=rc)

      end function __PROC(at_32)
#endif


      ! =======================
      !  of
      ! =======================
      function __PROC(of)(this, i) result(res)
         class(__vector), target, intent(in) :: this
         integer, intent(in) :: i
         __type_declare_result, pointer :: res

         res=>__GET(this%elements(i))
         return
      end function __PROC(of)


      ! =======================
      !  get
      ! =======================
      function __PROC(get_size_kind)(this, i) result(res)
         class(__vector), target, intent(in) :: this
         integer(kind=SIZE_KIND), intent(in) :: i
         __type_declare_component :: res
         integer(kind=SIZE_KIND) :: idx

         idx=merge(i, this%vsize+i, i>0)
         __TYPE_ASSIGN(res, __GET(this%elements(idx)))

      end function __PROC(get_size_kind)

#ifdef SUPPORT_FOR_INT64
      function __PROC(get_32)(this, i) result(res)
         class(__vector), target, intent(in) :: this
         integer, intent(in) :: i
         __type_declare_component :: res

! This should call get_size_kind(), but there is an ICE for
! gfortran 5.1
         integer(kind=SIZE_KIND) :: idx
         integer(kind=SIZE_KIND) :: i64
         i64 = int(i,kind=SIZE_KIND)
         idx=merge(i64, this%vsize+i64, i64>0)
         __TYPE_ASSIGN(res, __GET(this%elements(idx)))


      end function __PROC(get_32)
#endif


#ifndef __type_wrapped

      ! =======================
      !  get_data
      ! =======================
      function __PROC(get_data)(this) result(res)
         class(__vector), target, intent(in) :: this
         __declare_element_type, dimension(:), pointer :: res
         
         res=>this%elements(:this%vsize)
         return
      end function __PROC(get_data)
#endif
      
      ! =======================
      !  back
      ! =======================
      function __PROC(back)(this) result(res)
         class(__vector), target, intent(in) :: this
         __type_declare_result, pointer :: res
         
         res=>__GET(this%elements(this%vsize))
         return
      end function __PROC(back)
      
      ! =======================
      !  front
      ! =======================
      function __PROC(front)(this) result(res)
         class(__vector), target, intent(in) :: this
         __type_declare_result, pointer :: res
         
         res=>__GET(this%elements(1))
         return
      end function __PROC(front)
      
      
      ! =======================
      !  set
      ! =======================
      subroutine __PROC(set_size_kind)(this, i, value)
         class(__vector), intent(inout) :: this
         integer(kind=SIZE_KIND), intent(in) :: i
         __type_declare_dummy, intent(in) :: value

         integer(kind=SIZE_KIND) :: idx

         idx=merge(i, this%vsize+i, i>0)
         
         __TYPE_FREE(__GET(this%elements(idx)))
         __TYPE_ASSIGN(__GET(this%elements(idx)), value)
         return
      end subroutine __PROC(set_size_kind)

      subroutine __PROC(set_32)(this, i, value)
         class(__vector), intent(inout) :: this
         integer, intent(in) :: i
         __type_declare_dummy, intent(in) :: value

         call this%set(int(i,kind=SIZE_KIND), value)

      end subroutine __PROC(set_32)
      
      
      ! =======================
      !  reset
      ! =======================
      subroutine __PROC(reset)(this)
         class(__vector), intent(inout) :: this
         
         if (allocated(this%elements)) then
            deallocate(this%elements)
         end if
         this%vsize=0
         return
      end subroutine __PROC(reset)
      
      
#ifndef __type_wrapped
      ! =======================
      !  copyFromArray
      ! =======================
      subroutine __PROC(copyFromArray)(this, array)
         class(__vector), intent(inout) :: this
         __declare_element_type, target, intent(in) :: array(:)
         
         integer(kind=SIZE_KIND) :: n
         
         n = size(array)
         
         call this%reserve(n)
         this%elements(1:n) = array(1:n)
         this%vsize=n
         
         return
      end subroutine __PROC(copyFromArray)
#endif
      
      
#ifdef __use_equal_defined
      ! =======================
      !  get_index
      ! =======================
      function __PROC(get_index)(this, value) result(i)
         class(__vector), target, intent(in) :: this
         __type_declare_dummy, intent(in) :: value
         integer(kind=SIZE_KIND) :: i
         
         do i=1, this%vsize
            if (__TYPE_EQ(__GET(this%elements(i)), value)) return
         end do
         i=0
         return
      end function __PROC(get_index)
      
      
      ! =======================
      !  equal
      ! =======================
      __IMPURE_ELEMENTAL function                                                            & 
     &          __PROC(equal)(this, other) result(equal)
         logical :: equal
         class(__vector), intent(in) :: this, other
         integer :: i
         
         
         equal = (this%vsize == other%vsize) 
         if (equal) then
            equal = .false.
            do i = 1, this%vsize

       if (.not.                                                                              &
     &    __TYPE_EQ(__GET(this%elements(i)),__GET(other%elements(i))))                        &
     &       return

            end do
            equal = .true.
         endif
         
      end function __PROC(equal)
      
      ! =======================
      !  not_equal
      ! =======================
      __IMPURE_ELEMENTAL function                                                            &
     &        __PROC(not_equal)(this, other) result (not_equal)
         logical :: not_equal
         class(__vector), intent(in) :: this, other
         
         not_equal = .not. (this == other)
         
         return
      end function __PROC(not_equal)
      
#endif /* __use_equal_defined */

#ifdef __type_compare_well_defined      
      __IMPURE_ELEMENTAL function                                                          &
     &     __PROC(diff)(a, b, less) result(diff)
         class(__vector), intent(in) :: a
         class(__vector), intent(in) :: b
         logical, intent(in) :: less
         logical :: diff
         integer :: i, n

         __type_declare_result, pointer :: ai, bi

         n=min(a%size(), b%size())
         do i=1, n
            ai => a%of(i)
            bi => b%of(i)
           if (__TYPE_LESS_THAN(ai,bi)) then
             diff=less
             return
           else if (__TYPE_LESS_THAN(bi,ai)) then
             diff=.not.less
             return
           endif
         end do
         diff=merge(a%size()<b%size(), b%size()<a%size(), less)
         return
      end function __PROC(diff)

      __IMPURE_ELEMENTAL function                                                          &
     &     __PROC(less_than)(a, b) result(less_than)
         logical :: less_than
         class (__vector), intent(in) :: a
         class (__vector), intent(in) :: b

         less_than=__PROC(diff)(a, b, .true.)
         return
      end function __PROC(less_than)

      __IMPURE_ELEMENTAL function                                                                 &
     &       __PROC(greater_than_or_equal_to)(a, b)                                &
     &       result(greater_than_or_equal_to)
         logical :: greater_than_or_equal_to
         class (__vector), intent(in) :: a
         class (__vector), intent(in) :: b

         greater_than_or_equal_to=.not.__PROC(diff)(a, b, .true.)
         return
      end function __PROC(greater_than_or_equal_to)
      
      
      __IMPURE_ELEMENTAL function                                                                 &
     &                 __PROC(greater_than)(a, b) result(greater_than)
         logical :: greater_than
         class (__vector), intent(in) :: a
         class (__vector), intent(in) :: b

         greater_than=__PROC(diff)(a, b, .false.)
         return
       end function __PROC(greater_than)


      __IMPURE_ELEMENTAL function                                                             &
     &  __PROC(less_than_or_equal_to)(a, b)                                                   &
     &  result(less_than_or_equal_to)
         logical :: less_than_or_equal_to
         class (__vector), intent(in) :: a
         class (__vector), intent(in) :: b

         less_than_or_equal_to=.not.__PROC(diff)(a, b, .false.)
         return
      end function __PROC(less_than_or_equal_to)


#endif /* has defined less than */
      

      
      ! =======================
      !  push_back
      ! =======================
      subroutine __PROC(push_back)(this, value, unused, rc)
         class(__vector), intent(inout) :: this
         __type_declare_dummy, intent(in) :: value
         type (KeywordEnforcer), optional, intent(in) :: unused
         integer, optional, intent(out) :: rc

         _UNUSED_DUMMY(unused)

         call this%grow_to(this%vsize+1)
         call this%resize(this%vsize+1, value, rc=rc)
         return

      end subroutine __PROC(push_back)
      
      
      ! =======================
      !  pop_back
      ! =======================
      subroutine __PROC(pop_back)(this)
         class(__vector), intent(inout) :: this
         
         call this%downsize(this%vsize-1)
         return
      end subroutine __PROC(pop_back)
      
      
      ! =======================
      !  insert
      ! =======================
      subroutine __PROC(insert_size_kind)(                              &
     &   this, index, value, unused, rc)
         class(__vector), intent(inout) :: this

         integer(kind=SIZE_KIND), intent(in) :: index

         __type_declare_dummy, intent(in) :: value
         type (KeywordEnforcer), optional, intent(in) :: unused
         integer, optional, intent(out) :: rc

         __type_declare_component :: temp
         integer(kind=SIZE_KIND) :: i, n

         _UNUSED_DUMMY(unused)

         n = this%vsize
         if (index==n+1) then
           call this%push_back(value)
           return
         endif
         call this%grow_to(this%vsize+1)
         __TYPE_ASSIGN(temp,__GET(this%elements(n)))
         call this%resize(n+1, temp, rc=rc)
         
         do i = n-1, index, -1
          __TYPE_MOVE(__GET(this%elements(i+1)),__GET(this%elements(i)))
         end do
         
         __TYPE_ASSIGN(__GET(this%elements(index)), value)
         
         return
      end subroutine __PROC(insert_size_kind)
#ifdef SUPPORT_FOR_INT64      
      subroutine __PROC(insert_32)(this, index, value)
         class(__vector), intent(inout) :: this
         integer, intent(in) :: index
         __type_declare_dummy, intent(in) :: value
         
         call this%insert(int(index,kind=SIZE_KIND), value)
      end subroutine __PROC(insert_32)
#endif
      
      ! =======================
      !  resize
      ! =======================
      subroutine __PROC(resize_size)(this, newsize, value, unused, rc)
         class(__vector), intent(inout) :: this
         integer(kind=SIZE_KIND), intent(in) :: newsize
         __type_declare_dummy, optional, intent(in) :: value
         type (KeywordEnforcer), optional, intent(in) :: unused
         integer, optional, intent(out) :: rc

         integer(kind=SIZE_KIND) :: oldSize
         integer(kind=SIZE_KIND) :: i
         
         if (newSize == this%vsize) return
         if (newSize < 0) then
            if (present(rc)) rc = ILLEGAL_INPUT
            return
         end if
         oldSize=this%vsize
         
         call this%reserve(newSize)
         this%vsize = newSize

         do i = newSize + 1, oldSize
            __TYPE_FREE(__GET(this%elements(i)))
         end do
         
         if (present(value) .and. (newsize>oldsize)) then
            do i = oldSize + 1, newSize
               __TYPE_ASSIGN(__GET(this%elements(i)), value)
            end do
         endif
         return
      end subroutine __PROC(resize_size)

#ifdef SUPPORT_FOR_INT64
      subroutine __PROC(resize_32)(this, newsize, value, unused, rc)
         class(__vector), intent(inout) :: this
         integer, intent(in) :: newsize
         __type_declare_dummy, optional, intent(in) :: value
         type (KeywordEnforcer), optional, intent(in) :: unused
         integer, optional, intent(out) :: rc

         _UNUSED_DUMMY(unused)
         
         call this%resize(int(newsize,kind=SIZE_KIND), value, rc=rc)

      end subroutine __PROC(resize_32)
#endif


      ! =======================
      !  downsize
      ! =======================
      subroutine __PROC(downsize)(this, newsize)
         class(__vector), intent(inout) :: this
         integer(kind=SIZE_KIND), intent(in) :: newsize  ! assumes newsize<=size()
         integer(kind=SIZE_KIND) :: i

         if (newsize<this%vsize) then
           do i=newsize+1, this%vsize
              __TYPE_FREE(__GET(this%elements(i)))
           end do
           this%vsize=newsize
         endif

         return
      end subroutine __PROC(downsize)
      
      
      ! =======================
      !  clear
      ! =======================
      subroutine __PROC(clear)(this)
         class(__vector), intent(inout) :: this
         
         call this%downsize(0_SIZE_KIND)
         
         return
      end subroutine __PROC(clear)
      
      
      ! =======================
      !  shrink_to_fit
      ! =======================

      subroutine __PROC(shrink_to_fit)(this)
         class(__vector), intent(inout) :: this

         if (this%vsize<this%capacity()) then
           call this%set_capacity(this%vsize)
         endif
         return
      end subroutine __PROC(shrink_to_fit)
      

#ifdef __iterator
      ! =======================
      !  erase_one
      ! =======================
      subroutine __PROC(erase_one)(this, position)
         class(__vector), target, intent(inout) :: this
         type (__iterator), intent(in)  :: position
         
         call this%erase(position, position+1)
         
         return
      end subroutine __PROC(erase_one)
      
      
      ! =======================
      !  erase_range
      ! =======================
      subroutine __PROC(erase_range)(this, first, last)
         class(__vector), target, intent(inout) :: this
         type (__iterator), intent(in)  :: first
         type (__iterator), intent(in)  :: last

         integer :: i, delta

         delta=last%currentindex-first%currentindex
         do i=last%currentIndex, this%vsize
            associate(a => this%elements(i-delta),b =>this%elements(i))
              __TYPE_MOVE(__GET(a),__GET(b))
            end associate
         end do
         do i = this%vsize - delta + 1, last%currentIndex - 1
            __TYPE_FREE(__GET(this%elements(i)))
         end do
         this%vsize=this%vsize-delta

         return
      end subroutine __PROC(erase_range)
#endif
      
      ! =======================
      !  reserve
      ! =======================
      subroutine __PROC(reserve_size_kind)(this, capacity)
         class(__vector), intent(inout) :: this
         integer(kind=SIZE_KIND), intent(in) :: capacity

         if (capacity>this%capacity()) then
           call this%set_capacity(capacity)
         endif
         
         return
      end subroutine __PROC(reserve_size_kind)
#ifdef SUPPORT_FOR_INT64
      subroutine __PROC(reserve_32)(this, capacity)
         class(__vector), intent(inout) :: this
         integer, intent(in) :: capacity
         
         call this%reserve(int(capacity,kind=SIZE_KIND))
         return
      end subroutine __PROC(reserve_32)
#endif
      
      
      ! =======================
      !  set_capacity
      ! =======================
      subroutine __PROC(set_capacity)(this, capacity)
         class(__vector), intent(inout) :: this
         integer(kind=SIZE_KIND), intent(in) :: capacity  ! capacity must be >=0
         __declare_element_type, dimension(:), allocatable :: temp
         integer(kind=SIZE_KIND) :: i

         if (capacity>0) then                     ! capacity>0
           if (.not.allocated(this%elements)) then   ! not allocated
             allocate(this%elements(capacity))
           else                                      ! allocated
             allocate(temp(capacity))
             do i=1, this%vsize
               __TYPE_MOVE(__GET(temp(i)), __GET(this%elements(i)))
             end do
             call move_alloc(temp, this%elements)
           endif
         else if (allocated(this%elements)) then  ! capacity==0
            ! Note: vsize must be 0 to reach this point.
            deallocate(this%elements)
         endif

         return
      end subroutine __PROC(set_capacity)

      ! =======================
      !  grow_to
      ! =======================
      subroutine __PROC(grow_to)(this, capacity)
         class(__vector), intent(inout) :: this
         integer(kind=SIZE_KIND), intent(in) :: capacity

         if (capacity>this%capacity()) then
           call this%set_capacity(max(2*this%vsize, capacity)) ! gives O(n) algorithm for growing vector with push.
         endif
         return
      end subroutine __PROC(grow_to)

      ! =======================
      !  swap
      ! =======================
      subroutine __PROC(swap)(this, other)
         class(__vector), intent(inout) :: this
         type(__vector), intent(inout) :: other
         __declare_element_type,                                                                &
     &           dimension(:), allocatable :: tmpelementsfer
         integer :: tmpsize
         
         call move_alloc(this%elements, tmpelementsfer)
         call move_alloc(other%elements, this%elements)
         call move_alloc(tmpelementsfer, other%elements)
         tmpsize=this%vsize
         this%vsize=other%vsize
         other%vsize=tmpsize
         return
      end subroutine __PROC(swap)

      
#ifdef __iterator
      ! =======================
      !  begin - create an iterator
      ! =======================
      function __PROC(begin)(this) result(iter)
         type (__iterator) :: iter
         class (__vector), target, intent(in) :: this
         
         iter%currentIndex = 1
         if (allocated(this%elements)) then
            iter%elements => this%elements
         else
            iter%elements => null()
         end if
         
      end function __PROC(begin)
      
      
      ! =======================
      !  end_
      !  Construct  forward iterator, initially set to just
      !  after last element of vector.
      ! =======================
      function __PROC(end)(this) result(iter)
         class (__vector), target, intent(in) :: this
         type (__iterator) :: iter
         
         iter%currentIndex = this%size() + 1 ! past the end
         if (allocated(this%elements)) then
            iter%elements => this%elements
         else
            iter%elements => null()
         end if
         
      end function __PROC(end)
#endif      
      
#ifdef __riterator
      ! =======================
      !  rbegin - create a reverse iterator
      ! =======================
      function __PROC(rbegin)(this) result(iter)
         type (__riterator) :: iter
         class (__vector), target, intent(in) :: this
         
         iter%currentIndex = this%vsize
         if (allocated(this%elements)) then
            iter%elements => this%elements
         else
            iter%elements => null()
         end if
         
      end function __PROC(rbegin)
      
      
      ! =======================
      !  rend
      !  Construct  reverse iterator, initially set to just
      !  before first element of vector
      ! =======================
      function __PROC(rend)(this) result(iter)
         class (__vector), target, intent(in) :: this
         type (__riterator) :: iter
         
         iter%currentIndex = 0 ! before beginning
         if (allocated(this%elements)) then
            iter%elements => this%elements
         else
            iter%elements => null()
         end if
         
      end function __PROC(rend)
#endif


#ifdef __use_pointer
#  define __pointer_declare_result __type_declare_result
#  define __pointer_declare_dummy  __type_declare_dummy
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


#ifdef __iterator
#  include "vectorIterator_impl.inc"
#endif
#ifdef __riterator
#  include "vectorRiterator_impl.inc"
#endif

#undef __declare_element_type
#undef __GET
#include "templates/all_template_macros_undefs.inc"

!--------------------------------------------------------------------
! Copyright © 2020 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

! include map<integer,Key>
#include "parameters/Key/copy_omap_Key_to_map_Key.inc"

#define __map_T integer
#define __map_T_name "integer"
#define __map_T_KINDLEN(context) (kind=__gftl_size_kind)
#define __map_T_kindlen_string "(kind=__gftl_size_kind)"

#define __map __IDENTITY(__omap_guard)__IDENTITY(IndexMap)
#define __map_iterator __IDENTITY(__omap_guard)__IDENTITY(MapIterator)
#define __map_pair omap_MapPair

#define __map_set __omap_set
#define __map_set_node __omap_set_node
#define __map_set_iterator __omap_set_iterator
#define __map_guard __IDENTITY(__omap_guard)__IDENTITY(I_)

#include "map/procedures.inc"

#undef __map_set
#undef __map_set_node
#undef __map_set_iterator

#undef __map
#undef __map_pair
#undef __map_iterator
#undef __map_guard


#include "parameters/T/undef_map_T.inc"
#include "parameters/Key/undef_map_Key.inc"

   ! Declare Pair<Key,T>
#include "parameters/T1/copy_omap_Key_to_pair_T1.inc"
#include "parameters/T2/copy_omap_T_to_pair_T2.inc"
#define __pair __omap_pair
#define __pair_guard __IDENTITY(__omap_guard)__IDENTITY(p_)

#include "pair/procedures.inc"

#undef __pair
#undef __pair_guard

#include "parameters/T1/undef_pair_T1.inc"   
#include "parameters/T2/undef_pair_T2.inc"   




! include vector<Key>
#include "parameters/T/copy_omap_Key_to_vector_T.inc"
#define __vector_guard __IDENTITY(__omap_guard)__IDENTITY(K_)
#define __vector __IDENTITY(__vector_guard)__IDENTITY(Vector)
#define __vector_iterator __IDENTITY(__vector_guard)__IDENTITY(VectorIterator)
#define __vector_riterator __IDENTITY(__vector_guard)__IDENTITY(VectorRIterator)

#include "vector/procedures.inc"

#undef __vector
#undef __vector_iterator
#undef __vector_riterator
#undef __vector_guard
#include "parameters/T/undef_vector_T.inc"


! include vector<T>
#include "parameters/T/copy_omap_T_to_vector_T.inc"
#define __vector_guard __IDENTITY(__omap_guard)__IDENTITY(T_)
#define __vector __IDENTITY(__vector_guard)__IDENTITY(Vector)
#define __vector_iterator __IDENTITY(__vector_guard)__IDENTITY(VectorIterator)
#define __vector_riterator __IDENTITY(__vector_guard)__IDENTITY(T_VectorRIterator)

#include "vector/procedures.inc"

#undef __vector
#undef __vector_iterator
#undef __vector_riterator
#undef __vector_guard
#include "parameters/T/undef_vector_T.inc"


#define __guard __omap_guard


#include "parameters/Key/copy_omap_Key_to_internal_Key.inc"
#include "parameters/T/copy_omap_T_to_internal_T.inc"
#include "parameters/Key/define_derived_macros.inc"
#include "parameters/T/define_derived_macros.inc"


! This constructor is needed in situations where an empty dictionary needs to be
! passed to a procedure.  Prevents the need of declaring a local variable.
      function __MANGLE(new_omap_empty)() result(m)
         type (__omap) :: m

         if (.false.) print*,shape(m) ! avoid compiler warnings about unused
         
      end function __MANGLE(new_omap_empty)

      function __MANGLE(new_omap_copy)(x) result(m)
         type (__omap) :: m
         type (__omap), intent(in) :: x

         m%index_map = x%index_map
         m%keys = x%keys
         m%values = x%values

      end function __MANGLE(new_omap_copy)

      function __MANGLE(new_omap_initializer_list)(il) result(m)
         type (__omap) :: m
         type (__omap_pair), intent(in) :: il(:)
         
         integer :: i

         do i = 1, size(il)
            call m%insert(il(i)%first, il(i)%second)
         end do
         
      end function __MANGLE(new_omap_initializer_list)

! =======================
!  empty
! =======================
      logical function __MANGLE(empty)(this) result(isEmpty)
         class (__omap), intent(in) :: this

         isEmpty = this%index_map%empty()

      end function __MANGLE(empty)


! =======================
!  size
! =======================
      function __MANGLE(size)(this) result(size)
         integer(kind=GFTL_SIZE_KIND) :: size
         class (__omap), intent(in) :: this

         size = this%index_map%size()

      end function __MANGLE(size)


! =======================
!  max_size
! =======================
!  limited by 32 bit integer in terms of result
      function __MANGLE(max_size)() result(max_size)
         integer(kind=GFTL_SIZE_KIND) :: max_size

         max_size = huge(1_GFTL_SIZE_KIND)

      end function __MANGLE(max_size)


! =======================
!  insert
! =======================
      subroutine __MANGLE(insert_key_value)(this, key, value)
         class (__omap), intent(inout) :: this
         __Key_declare_dummy__, intent(in) :: key
         __T_declare_dummy__, intent(in) :: value

         integer(kind=GFTL_SIZE_KIND), pointer :: idx
         integer :: status

         idx => this%index_map%at(key, rc=status)
         if (status /= success) then
            allocate(idx)
            idx = this%keys%size() + 1
            call this%index_map%insert(key, idx)
            call this%keys%push_back(key)
            call this%values%push_back(value)
            deallocate(idx)
         end if

      end subroutine __MANGLE(insert_key_value)


!!$      ! TODO: This implemetation requires to searches for
!!$      ! the element.  Should be improved
!!$      subroutine __MANGLE(insert_pair)(this, p)
!!$         class (__omap), intent(inout) :: this
!!$         type (__omap_pair), intent(in) :: p
!!$
!!$         call this%insert_key_value(p%first, p%second)
!!$
!!$      end subroutine __MANGLE(insert_pair)

! =======================
!  
! =======================
      function __MANGLE(get)(this, key, value) result(res)
      logical :: res
      class(__omap), target, intent(in) :: this
      __Key_declare_dummy__ :: key
      __T_declare_result__, pointer, intent(out) :: value
      type(__omap_pair) :: p

      type(__omap_pair), pointer :: pair_ptr

      integer(kind=GFTL_SIZE_KIND), pointer :: idx

      idx => this%index_map%at(key)
      res = associated(idx)
      if (res) then
         value => this%values%at(idx)
      else
         value => null()
      end if

      return
      end function __MANGLE(get)

! =======================
!  set
! =======================
      subroutine __MANGLE(set_)(this, key, value)
      class(__omap), intent(inout) :: this
      __Key_declare_dummy__, intent(in) :: key
      __T_declare_dummy__, intent(in) :: value

      type(__omap_pair) :: p
      integer :: status
      integer(kind=__gftl_size_kind), pointer :: idx
      
      idx => this%index_map%at(key, status)
      if (rc == SUCCESS) then
         call this%values%set(idx, value)
      else
         call this%insert(key, value)
      end if

      return

      end subroutine __MANGLE(set_)

! =======================

!  Analog of C++ [] operator.  Would like to grow container if key is
!  not found, but if element is deferred, there is no good way to do
!  this.   And it would be confusing to have differing semantics.
!  Use the set() method.
! =======================
      function __MANGLE(of)(this, key) result(res)
      class(__omap), target, intent(inout) :: this
      __Key_declare_dummy__, intent(in) :: key
      __T_declare_result__, pointer :: res
      
      integer(kind=__gftl_size_kind), pointer :: idx
      
      idx => this%index_map%of(key)
      if (associated(idx)) then
         res => this%values%of(idx)
      else
         res => null()
      end if

      return
      end function __MANGLE(of)

! =======================
!  at
! =======================
      function __MANGLE(at_rc)(this, key, rc) result(res)
      __T_declare_result__, pointer :: res
      class(__omap), target, intent(in) :: this
      __Key_declare_dummy__, intent(in) :: key
      integer, intent(out) :: rc

      type (__omap_iterator) :: iter

      integer(kind=__gftl_size_kind), pointer :: idx
      
      idx => this%index_map%of(key)
      if (associated(idx)) then
         res => this%values%of(idx)
         rc = SUCCESS
      else
         res => null()
         rc = OUT_OF_RANGE
      end if

      return
      end function __MANGLE(at_rc)



! =======================
!  erase_iter()
! =======================
      function __MANGLE(erase_iter)(this, iter) result(new_iter)
         type(__omap_iterator) :: new_iter
         class(__omap), intent(inout) :: this
         type(__omap_iterator), intent(in) :: iter

         call this%index_map%erase(iter%index_map_iter)
         call this%keys%erase(iter%keys_iter)
         call this%values%erase(iter%keys_iter)

      end function __MANGLE(erase_iter)

! =======================
!  erase_key()
! =======================
      function __MANGLE(erase_key)(this, k) result(n)
         integer(kind=GFTL_SIZE_KIND) :: n
         class(__omap), intent(inout) :: this
         __Key_declare_dummy__, intent(in) :: k

         type(__omap_iterator) :: iter

         iter = this%find(k)
         if (iter /= this%end()) then
            iter = this%erase(iter)
            n = 1
         else
            n = 0
         end if

      end function __MANGLE(erase_key)


! =======================
!  erase_range()
! =======================
      function __MANGLE(erase_range)(this, first, last) result(new_iter)
         type(__omap_iterator) :: new_iter
         class(__omap), target, intent(inout) :: this
         type(__omap_iterator), intent(in) :: first
         type(__omap_iterator), intent(in) :: last

         new_iter%reference => first%reference
!!$         new_iter%set_iter = this%tree%erase(first%set_iter, last%set_iter)

      end function __MANGLE(erase_range)


! =======================
!  clear
! =======================
      subroutine __MANGLE(clear)(this)
      class(__omap), intent(inout) :: this

      call this%index_map%clear()
      call this%keys%clear()
      call this%values%clear()

      end subroutine __MANGLE(clear)


! Non type-bound functions
      

   ! =======================
   !  equal - note ignores order
   ! =======================
   logical function __MANGLE(equal)(a, b) result(equal)
      type(__omap), intent(in) :: a
      type(__omap), intent(in) :: b

!!$      equal = a%tree == b%tree

   end function __MANGLE(equal)

   ! =======================
   !  not_equal
   ! =======================
   logical function __MANGLE(not_equal)(a, b) result(not_equal)
      type(__omap), intent(in) :: a
      type(__omap), intent(in) :: b

      not_equal = .not. (a == b)

   end function __MANGLE(not_equal)

! =======================
!  begin
! =======================
      function __MANGLE(begin)(this) result(iter)
         class(__omap), target, intent(in) :: this
         type (__omap_iterator) :: iter

         iter%reference => this
!!$         iter%set_iter = this%tree%begin()

      end function __MANGLE(begin)


! =======================
!  end
! =======================
      function __MANGLE(end)(this) result(iter)
         class(__omap), target, intent(in) :: this
         type (__omap_iterator) :: iter

         iter%reference => this
!!$         iter%set_iter = this%tree%end()

      end function __MANGLE(end)


! =======================
!  find
! =======================
      function __MANGLE(find)(this, key) result(iter)
         type (__omap_iterator) :: iter
         class(__omap), target, intent(in) :: this
         __Key_declare_dummy__, intent(in) :: key

         type (__omap_pair) :: p

         __Key_COPY__(p%first, key)

         iter%reference => this
!!$         iter%set_iter = this%tree%find(p)

      end function __MANGLE(find)


! =======================
!  count
! =======================

      function __MANGLE(count)(this, key) result(count)
         integer(kind=GFTL_SIZE_KIND) :: count
         class(__omap), intent(in) :: this
         __Key_declare_dummy__, intent(in) :: key

         type (__omap_pair) :: p

         __Key_COPY__(p%first, key)

!!$         count = this%tree%count(p)

      end function __MANGLE(count)


! =======================
!  copyFrom
! =======================

      subroutine __MANGLE(deep_copy)(this, x)
         class(__omap), intent(out) :: this
         type(__omap), intent(in) :: x

         ! Set container assigmnent ovrrides default assignment.
         ! Correct implementations should do a deep copy here
!!$         this%tree = x%tree

      end subroutine __MANGLE(deep_copy)
      

      logical function __MANGLE(key_less_than)(a,b) result(less_than)
         type(__omap_pair), intent(in) :: a
         type(__omap_pair), intent(in) :: b

         less_than = __Key_LT__(a%get_first(),b%get_first())

         return
      end function __MANGLE(key_less_than)

#ifdef _DUMP_MAP
! =======================
!  mapdump
! =======================
      subroutine mapdump(this)
      class(__omap), intent(inout) :: this
      type(__omap_set_iterator) :: it
      type(__omap_pair), pointer :: p

!!$      it=this%tree%begin()
      do while (it%good())
        p => it%at()
        write(*, *)p%first, ' => ',p%secord,'+'
        call it%next
      end do
      return
      end subroutine mapdump
#endif

#include "ordered_map/iterator_procedures.inc"



   ! undef derived template parameters
#include "parameters/Key/undef_derived_macros.inc"
#include "parameters/T/undef_derived_macros.inc"
   ! undef internal template parameters
#undef __guard
#include "parameters/Key/undef_internal.inc"
#include "parameters/T/undef_internal.inc"
   

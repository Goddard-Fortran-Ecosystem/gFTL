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

#define __pair __map_pair
#define __pair_guard __IDENTITY(__map_guard)__IDENTITY(p_)
   
#include "parameters/Key/copy_map_Key_to_internal_Key.inc"
#include "parameters/T/copy_map_T_to_internal_T.inc"
#include "parameters/Key/define_derived_macros.inc"
#include "parameters/T/define_derived_macros.inc"

#if (defined(__Key_LT__) || defined(__Key_EQ__)) && (defined(__T_LT__) || defined(__T_EQ__))
!  aloha
#    define __map_pair_EQ(lhs,rhs) (lhs==rhs)
#endif
#if defined(__Key_LT__) && defined(__T_LT__)
#    define __map_pair_LT(lhs,rhs) (lhs<rhs)
#endif
 
#include "parameters/Key/undef_derived_macros.inc"
#include "parameters/T/undef_derived_macros.inc"
#include "parameters/Key/undef_internal.inc"
#include "parameters/T/undef_internal.inc"

#include "parameters/T1/copy_map_Key_to_pair_T1.inc"
#include "parameters/T2/copy_map_T_to_pair_T2.inc"

#include "pair/procedures.inc"

#undef __pair
#undef __pair_guard


#define __set __IDENTITY(__map_guard)__IDENTITY(__map_set)
#define __set_iterator __IDENTITY(__map_guard)__IDENTITY(__map_set_iterator)
#define __set_T __map_pair
#define __set_T_LT(a,b) __IDENTITY(__map_guard)__IDENTITY(key_less_than)(a,b)
#define __set_guard __IDENTITY(__map_guard)__IDENTITY(s_)
#include "set/procedures.inc"

#undef __set
#undef __set_iterator
#include "parameters/T/undef_set_T.inc"
#undef __set_guard
#undef __set_T_COPY

#include "parameters/T1/undef_pair_T1.inc"   
#include "parameters/T2/undef_pair_T2.inc"   

#define __guard __map_guard


#include "parameters/Key/copy_map_Key_to_internal_Key.inc"
#include "parameters/T/copy_map_T_to_internal_T.inc"
#include "parameters/Key/define_derived_macros.inc"
#include "parameters/T/define_derived_macros.inc"

   subroutine __MANGLE(unused_procedure)()
      ! The following are an adhoc way to suppress compiler
      ! warnings about parameters not used in this template
      __UNUSED_DUMMY(BAD_ALLOC)
      __UNUSED_DUMMY(ILLEGAL_INPUT)
      __UNUSED_DUMMY(LENGTH_ERROR)
      __UNUSED_DUMMY(TYPE_HAS_NO_DEFAULT_VALUE)
      __UNUSED_DUMMY(NO_TYPE__)
   end subroutine __MANGLE(unused_procedure)

! This constructor is needed in situations where an empty dictionary needs to be
! passed to a procedure.  Prevents the need of declaring a local variable.
      function __MANGLE(new_map_empty)() result(m)
         type (__map) :: m

         m%tree = __MANGLE(Set)()
      end function __MANGLE(new_map_empty)

      function __MANGLE(new_map_copy)(x) result(m)
         type (__map) :: m
         type (__map), intent(in) :: x

         m%tree = x%tree
      end function __MANGLE(new_map_copy)

      function __MANGLE(new_map_initializer_list)(il) result(m)
         type (__map) :: m
         type (__map_pair), intent(in) :: il(:)
         
         integer(kind=GFTL_SIZE_KIND) :: i

         m = __map()
         do i = 1, size(il,kind=GFTL_SIZE_KIND)
            call m%insert(il(i))
         end do
         
      end function __MANGLE(new_map_initializer_list)

! =======================
!  empty
! =======================
      logical function __MANGLE(empty)(this) result(isEmpty)
         class (__map), intent(in) :: this

         isEmpty = this%tree%empty()

      end function __MANGLE(empty)


! =======================
!  size
! =======================
      function __MANGLE(size)(this) result(size)
         integer(kind=GFTL_SIZE_KIND) :: size
         class (__map), intent(in) :: this

         size = this%tree%size()

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
      recursive subroutine __MANGLE(insert_key_value)(this, key, value)
         class (__map), intent(inout) :: this
         __Key_declare_dummy__, intent(in) :: key
         __T_declare_dummy__, target, intent(in) :: value

         integer, save :: depth = 0

         type (__map_pair) :: p

         depth = depth + 1
         __Key_COPY__(p%first, key)
         __T_COPY__(p%second, value)

         call this%tree%insert(p)
         depth = depth - 1

      end subroutine __MANGLE(insert_key_value)


      recursive subroutine __MANGLE(insert_pair)(this, p)
         class (__map), intent(inout) :: this
         type (__map_pair), intent(in) :: p

         integer, save :: depth = 0

         depth = depth + 1
         call this%tree%insert(p)

         depth = depth - 1
      end subroutine __MANGLE(insert_pair)

      ! =======================
      !  set
      ! =======================
      subroutine __MANGLE(set_)(this, key, value)
         class(__map), intent(inout) :: this
         __Key_declare_dummy__, intent(in) :: key
         __T_declare_dummy__, intent(in) :: value
         type(__map_pair) :: p

         __Key_COPY__(p%first, key)
         __T_COPY__(p%second, value)

         call this%tree%insert(p)
         return

      end subroutine __MANGLE(set_)

      ! =======================
      !  of - grows map if key does not exist
      !  Analog of C++ [] operator.
      ! =======================
      function __MANGLE(of)(this, key) result(res)
         class(__map), target, intent(inout) :: this
         __Key_declare_dummy__, intent(in) :: key
         __T_declare_result__, pointer :: res
         type(__map_pair) :: p

         logical :: is_new
         type(__MANGLE(SetIterator)) :: iter
         type(__map_pair), pointer :: pair_ptr

         __Key_COPY__(p%first, key)

         call this%tree%insert(p, iter=iter, is_new=is_new)
         if (.not. is_new) then
            pair_ptr => iter%of()
            res => pair_ptr%second
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
         class(__map), target, intent(in) :: this
         __Key_declare_dummy__, intent(in) :: key
         integer, intent(out) :: rc

         type (__map_iterator) :: iter

         iter = this%find(key)
         if (iter == this%end()) then
            res => null()
            rc = OUT_OF_RANGE
         else
            res => iter%second()
            rc = SUCCESS
         end if

         return
      end function __MANGLE(at_rc)



      ! =======================
      !  erase_iter()
      ! =======================
      function __MANGLE(erase_iter)(this, iter) result(new_iter)
         type(__map_iterator) :: new_iter
         class(__map), intent(inout) :: this
         type(__map_iterator), intent(in) :: iter

         new_iter%reference => iter%reference
         new_iter%set_iter = this%tree%erase(iter%set_iter)

      end function __MANGLE(erase_iter)

      ! =======================
      !  erase_key()
      ! =======================
      function __MANGLE(erase_key)(this, k) result(n)
         integer(kind=GFTL_SIZE_KIND) :: n
         class(__map), intent(inout) :: this
         __Key_declare_dummy__, intent(in) :: k

         type(__map_iterator) :: iter

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
         type(__map_iterator) :: new_iter
         class(__map), target, intent(inout) :: this
         type(__map_iterator), intent(in) :: first
         type(__map_iterator), intent(in) :: last

         new_iter%reference => first%reference
         new_iter%set_iter = this%tree%erase(first%set_iter, last%set_iter)

      end function __MANGLE(erase_range)


      ! =======================
      !  clear
      ! =======================
      recursive subroutine __MANGLE(clear)(this)
         class(__map), intent(inout) :: this

         call this%tree%clear()

      end subroutine __MANGLE(clear)


      ! Non type-bound functions


      ! =======================
      !  equal
      ! =======================
      logical function __MANGLE(equal)(a, b) result(equal)
         type(__map), intent(in) :: a
         type(__map), intent(in) :: b

         equal = a%tree == b%tree

      end function __MANGLE(equal)

      ! =======================
      !  not_equal
      ! =======================
      logical function __MANGLE(not_equal)(a, b) result(not_equal)
         type(__map), intent(in) :: a
         type(__map), intent(in) :: b

         not_equal = .not. (a == b)

      end function __MANGLE(not_equal)

      ! =======================
      !  begin
      ! =======================
      function __MANGLE(begin)(this) result(iter)
         class(__map), target, intent(in) :: this
         type (__map_iterator) :: iter

         iter%reference => this
         iter%set_iter = this%tree%begin()

      end function __MANGLE(begin)


! =======================
!  end
! =======================
      function __MANGLE(end)(this) result(iter)
         class(__map), target, intent(in) :: this
         type (__map_iterator) :: iter

         iter%reference => this
         iter%set_iter = this%tree%end()

      end function __MANGLE(end)


! =======================
!  find
! =======================
      function __MANGLE(find)(this, key) result(iter)
         type (__map_iterator) :: iter
         class(__map), target, intent(in) :: this
         __Key_declare_dummy__, intent(in) :: key

         type (__map_pair) :: p

         __Key_COPY__(p%first, key)

         iter%reference => this
         iter%set_iter = this%tree%find(p)

      end function __MANGLE(find)


! =======================
!  count
! =======================

      function __MANGLE(count)(this, key) result(count)
         integer(kind=GFTL_SIZE_KIND) :: count
         class(__map), intent(in) :: this
         __Key_declare_dummy__, intent(in) :: key

         type (__map_pair) :: p

         __Key_COPY__(p%first, key)

         count = this%tree%count(p)

      end function __MANGLE(count)


! =======================
!  copyFrom
! =======================

      recursive subroutine __MANGLE(deep_copy)(this, x)
         class(__map), intent(out) :: this
         type(__map), target, intent(in) :: x

         integer, save :: depth = 0

         ! Set container assigmnent ovrrides default assignment.
         ! Correct implementations should do a deep copy here
!!$         this%tree = x%tree

         depth = depth + 1
         call this%tree%deep_copy(x%tree)

         depth = depth - 1

      end subroutine __MANGLE(deep_copy)
      

      logical function __MANGLE(key_less_than)(a,b) result(less_than)
         type(__map_pair), intent(in) :: a
         type(__map_pair), intent(in) :: b

         less_than = __Key_LT__(a%first,b%first)

         return
      end function __MANGLE(key_less_than)

      recursive logical function __MANGLE(verify)(this) result(verify)
         class (__map), target, intent(in) :: this

         verify = this%tree%verify()
      end function __MANGLE(verify)


#ifdef _DUMP_MAP
! =======================
!  mapdump
! =======================
      subroutine mapdump(this)
      class(__map), intent(inout) :: this
      type(__map_set_iterator) :: it
      type(__map_pair), pointer :: p

      it=this%tree%begin()
      do while (it%good())
        p => it%of()
        write(*, *)p%first, ' => ',p%secord,'+'
        call it%next
      end do
      return
      end subroutine mapdump
#endif

#include "map/iterator_procedures.inc"



   ! undef derived template parameters
#include "parameters/Key/undef_derived_macros.inc"
#include "parameters/T/undef_derived_macros.inc"
   ! undef internal template parameters
#undef __guard
#include "parameters/Key/undef_internal.inc"
#include "parameters/T/undef_internal.inc"

#include "parameters/T/copy_map_pair_to_algorithm_T.inc"
#define __algorithm_guard __IDENTITY(__map_guard)
#define __algorithm_container __map
#define __algorithm_iterator __map_iterator
#include "algorithms/procedures.inc"
#undef __algorithm_container
#undef __algorithm_iterator
#undef __algorithm_guard
#include "parameters/T/undef_algorithm_T.inc"

#undef __pair
#undef __pair_guard
   
#ifdef __map_pair_EQ
#    undef __map_pair_EQ
#endif
#ifdef __map_pair_LT
#    undef __map_pair_LT
#endif

!--------------------------------------------------------------------
! Copyright © 2020 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

   ! Declare map<T,Key>
#include "parameters/Key/copy_omap_Key_to_map_Key.inc"
#include "parameters/T/copy_omap_T_to_map_T.inc"

#define __map __IDENTITY(__omap_guard)__IDENTITY(Map)
#define __map_iterator __IDENTITY(__omap_guard)__IDENTITY(MapIterator)
#define __map_pair __omap_pair

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

#include "parameters/Key/undef_map_Key.inc"
#include "parameters/T/undef_map_T.inc"

   ! Declare vector<Key>
   ! __T   __T_declare_component__
#include "parameters/T/copy_omap_Key_to_vector_T.inc"
#define __vector_guard __IDENTITY(__omap_guard)__IDENTITY(K_)
#define __vector __IDENTITY(__omap_guard)__IDENTITY(Vector)
#define __vector_iterator __IDENTITY(__omap_guard)__IDENTITY(VectorIterator)
#define __vector_ftn_iterator __IDENTITY(__omap_guard)__IDENTITY(VectorFtnIterator)
#define __vector_riterator __IDENTITY(__omap_guard)__IDENTITY(VectorRIterator)

#include "vector/procedures.inc"

#undef __vector
#undef __vector_iterator
#undef __vector_ftn_iterator
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

         m%map = __MANGLE(Map)()
         m%ordered_keys = __MANGLE(Vector)()
         
      end function __MANGLE(new_omap_empty)

      function __MANGLE(new_omap_copy)(x) result(m)
         type (__omap) :: m
         type (__omap), intent(in) :: x

         m%map = x%map
         m%ordered_keys = x%ordered_keys

      end function __MANGLE(new_omap_copy)

      function __MANGLE(new_omap_initializer_list)(il) result(m)
         type (__omap) :: m
         type (__omap_pair), intent(in) :: il(:)
         
         integer(kind=GFTL_SIZE_KIND) :: i

         do i = 1, size(il,kind=GFTL_SIZE_KIND)
            call m%insert(il(i)%first, il(i)%second)
         end do
         
      end function __MANGLE(new_omap_initializer_list)

! =======================
!  empty
! =======================
      logical function __MANGLE(empty)(this) result(isEmpty)
         class (__omap), intent(in) :: this

         isEmpty = this%map%empty()

      end function __MANGLE(empty)


! =======================
!  size
! =======================
      function __MANGLE(size)(this) result(size)
         integer(kind=GFTL_SIZE_KIND) :: size
         class (__omap), intent(in) :: this

         size = this%map%size()

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
         class (__omap), intent(inout) :: this
         __Key_declare_dummy__, intent(in) :: key
         __T_declare_dummy__, intent(in) :: value

         integer :: status
         __T_declare_result__, pointer :: tmp

         tmp => this%map%at(key, rc=status)
         if (status /= success) then ! new
            call this%ordered_keys%push_back(key)
            call this%map%insert(key, value)
         end if

         __UNUSED_DUMMY(tmp)
      end subroutine __MANGLE(insert_key_value)


      ! TODO: This implemetation requires to searches for
      ! the element.  Should be improved
      recursive subroutine __MANGLE(insert_pair)(this, p)
         class (__omap), intent(inout) :: this
         type (__omap_pair), intent(in) :: p

         call this%insert(p%first, p%second)

      end subroutine __MANGLE(insert_pair)

! =======================
!  set
! =======================
      subroutine __MANGLE(set_)(this, key, value)
      class(__omap), intent(inout) :: this
      __Key_declare_dummy__, intent(in) :: key
      __T_declare_dummy__, intent(in) :: value

      integer :: status
      __T_declare_result__, pointer :: v
      
      v => this%map%at(key, status)
      if (status == SUCCESS) then ! overwrite
         call this%map%set(key, value)
      else ! new 
         call this%map%insert(key, value)
         call this%ordered_keys%push_back(key)
      end if

      return
      __UNUSED_DUMMY(v)
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

      res => this%map%of(key)

      return
      end function __MANGLE(of)

! =======================
!  at
! =======================
      function __MANGLE(at_rc)(this, key, rc) result(res)
      __T_declare_result__, pointer :: res
      class(__omap), target, intent(in) :: this
      __Key_declare_dummy__, intent(in) :: key
      integer, intent(out), optional :: rc

      res => this%map%at(key, rc)

      return
      end function __MANGLE(at_rc)



! =======================
!  erase_iter()
! =======================
      function __MANGLE(erase_iter)(this, iter) result(new_iter)
         type(__omap_iterator) :: new_iter
         class(__omap), intent(inout) :: this
         type(__omap_iterator), intent(in) :: iter

         type(__MANGLE(MapIterator)) :: m_iter

         m_iter = this%map%erase(this%map%find(iter%key_iter%of()))
         new_iter%key_iter = this%ordered_keys%erase(iter%key_iter)

         __UNUSED_DUMMY(m_iter)
      end function __MANGLE(erase_iter)

! =======================
!  erase_key()
! =======================
      function __MANGLE(erase_key)(this, k) result(n)
         integer(kind=GFTL_SIZE_KIND) :: n
         class(__omap), intent(inout) :: this
         __Key_declare_dummy__, intent(in) :: k

         __T_declare_result__, pointer :: v
         type(__MANGLE(VectorIterator)) :: iter

         v => this%of(k)
         if (associated(v)) then
            n = this%map%erase(k)
            associate (b => this%ordered_keys%begin(), e => this%ordered_keys%end())
              iter = this%ordered_keys%erase(find(b, e, k))
            end associate
         else
            n = 0
         end if

         return
         __UNUSED_DUMMY(iter)
      end function __MANGLE(erase_key)


! =======================
!  erase_range()
! =======================
      function __MANGLE(erase_range)(this, first, last) result(new_iter)
         type(__omap_iterator) :: new_iter
         class(__omap), target, intent(inout) :: this
         type(__omap_iterator), intent(in) :: first
         type(__omap_iterator), intent(in) :: last

         type(__omap_iterator) :: iter
         
         iter = first
         do while (iter /= last)
            iter = this%erase(iter) ! advances iter
         end do
         new_iter = iter

      end function __MANGLE(erase_range)


! =======================
!  clear
! =======================
      recursive subroutine __MANGLE(clear)(this)
      class(__omap), intent(inout) :: this

      call this%map%clear()
      call this%ordered_keys%clear()

      end subroutine __MANGLE(clear)


! Non type-bound functions
      

   ! =======================
   !  equal - note ignores order
   ! =======================
   logical function __MANGLE(equal)(a, b) result(equal)
      type(__omap), intent(in) :: a
      type(__omap), intent(in) :: b

      equal = (a%map == b%map)

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
         iter%key_iter = this%ordered_keys%begin()

      end function __MANGLE(begin)


! =======================
!  end
! =======================
      function __MANGLE(end)(this) result(iter)
         class(__omap), target, intent(in) :: this
         type (__omap_iterator) :: iter

         iter%reference => this
         iter%key_iter = this%ordered_keys%end()

      end function __MANGLE(end)

! =======================
!  ftn_begin
! =======================
      function __MANGLE(ftn_begin)(this) result(iter)
         class(__omap), target, intent(in) :: this
         type (__omap_iterator) :: iter

         iter%reference => this
         iter%key_iter = this%ordered_keys%ftn_begin()

      end function __MANGLE(ftn_begin)


! =======================
!  ftn_end
! =======================
      function __MANGLE(ftn_end)(this) result(iter)
         class(__omap), target, intent(in) :: this
         type (__omap_iterator) :: iter

         iter%reference => this
         iter%key_iter = this%ordered_keys%ftn_end()

      end function __MANGLE(ftn_end)


! =======================
!  find
! =======================
      function __MANGLE(find)(this, key) result(iter)
         type (__omap_iterator) :: iter
         class(__omap), target, intent(in) :: this
         __Key_declare_dummy__, intent(in) :: key


         iter%reference => this
         associate (b => this%ordered_keys%begin(), e => this%ordered_keys%end())
           iter%key_iter = find(b, e, key) ! STL::algorithms::find()
         end associate

      end function __MANGLE(find)


! =======================
!  count
! =======================

      function __MANGLE(count)(this, key) result(count)
         integer(kind=GFTL_SIZE_KIND) :: count
         class(__omap), intent(in) :: this
         __Key_declare_dummy__, intent(in) :: key

         count = this%map%count(key)

      end function __MANGLE(count)


! =======================
!  copyFrom
! =======================

      recursive subroutine __MANGLE(deep_copy)(this, x)
         class(__omap), intent(out) :: this
         type(__omap), intent(in) :: x

         ! Set container assigmnent ovrrides default assignment.
         ! Correct implementations should do a deep copy here
         this%map = x%map
         this%ordered_keys = x%ordered_keys

      end subroutine __MANGLE(deep_copy)
      

      logical function __MANGLE(key_less_than)(a,b) result(less_than)
         type(__omap_pair), intent(in) :: a
         type(__omap_pair), intent(in) :: b

         associate (ka => a%first, kb => b%first)
           less_than = __Key_LT__(ka, kb)
         end associate
           

         return
      end function __MANGLE(key_less_than)

      recursive logical function __MANGLE(verify)(this) result(verify)
         class (__omap), target, intent(in) :: this

         verify = this%map%verify()
      end function __MANGLE(verify)


      recursive subroutine __MANGLE(final)(this)
         type(__omap), intent(inout) :: this

         call this%clear()
      end subroutine __MANGLE(final)



#include "ordered_map/iterator_procedures.inc"



   ! undef derived template parameters
#include "parameters/Key/undef_derived_macros.inc"
#include "parameters/T/undef_derived_macros.inc"
   ! undef internal template parameters
#undef __guard
#include "parameters/Key/undef_internal.inc"
#include "parameters/T/undef_internal.inc"
   

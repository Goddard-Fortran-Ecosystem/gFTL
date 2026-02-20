#include "parameters/T/copy_stack_T_to_deque_T.inc"

#define __deque __stack_container_type
#define __deque_guard __IDENTITY(__stack_guard)__IDENTITY(d_)
   
#include "deque/procedures.inc"

#undef __deque
#undef __deque_guard


#include "parameters/T/copy_stack_T_to_internal_T.inc"
#define __guard __stack_guard

! define derived generic template parameters from internal parameters.
#include "parameters/T/define_derived_macros.inc"



   ! =======================
   !  Structure constructors
   ! =======================
   function __MANGLE(new_stack_empty)() result(q)
      type (__stack) :: q
      logical, parameter :: flag = .false.

      if (flag) print*,shape(q) ! avoid warning about unused return value

      return
   end function __MANGLE(new_stack_empty)

   function __MANGLE(new_stack_container)(container) result(q)
      type (__stack) :: q
      type(__stack_container_type), intent(in) :: container

      q%container = container

      return
   end function __MANGLE(new_stack_container)



   function __MANGLE(new_stack_copy)(x) result(q)
      type (__stack) :: q
      type (__stack) :: x

      integer(kind=GFTL_SIZE_KIND) :: i

      q%container = x%container
      return
   end function __MANGLE(new_stack_copy)



   ! =======================
   !  size
   ! =======================
   pure function __MANGLE(size)(this) result(res)
      class(__stack), intent(in) :: this
      integer(kind=GFTL_SIZE_KIND) :: res

      res = this%container%size()

      return
   end function __MANGLE(size)

   
   
   ! =======================
   !  empty() - is container empty?
   ! =======================
   pure logical function __MANGLE(empty)(this) result(empty)
      class(__stack), intent(in) :: this

      empty = this%container%empty()

   end function __MANGLE(empty)

   
   ! =======================
   !  top() - last element; undefined behavior for empty container
   ! =======================
   function __MANGLE(top)(this) result(res)
      class(__stack), target, intent(in) :: this
      __T_declare_result__, pointer :: res

      res => this%container%back()

      return
   end function __MANGLE(top)


   ! =======================
   !  push()
   ! =======================
   subroutine __MANGLE(push)(this, value, unused, rc)
      class(__stack), intent(inout) :: this
      __T_declare_dummy__, intent(in) :: value
      type (KeywordEnforcer), optional, intent(in) :: unused
      integer, optional, intent(out) :: rc

      __UNUSED_DUMMY(unused)


      call this%container%push_back(value, rc=rc)

   end subroutine __MANGLE(push)


   ! =======================
   !  pop()
   ! =======================
   subroutine __MANGLE(pop)(this)
      class(__stack), intent(inout) :: this

      call this%container%pop_back()

      return
   end subroutine __MANGLE(pop)


#if defined(__T_EQ__) || defined(__T_LT__)
   ! =======================
   !  equal
   ! =======================
   impure elemental function __MANGLE(equal)(lhs, rhs) result(equal)
      logical :: equal
      type(__stack), intent(in) :: lhs
      type(__stack), intent(in) :: rhs


      equal = (lhs%container == rhs%container)

      return
   end function __MANGLE(equal)

   ! =======================
   !  not_equal
   ! =======================
   impure elemental function __MANGLE(not_equal)(lhs, rhs) result (not_equal)
      logical :: not_equal
      type(__stack), intent(in) :: lhs
      type(__stack), intent(in) :: rhs

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
      type (__stack), intent(in) :: lhs
      type (__stack), intent(in) :: rhs

      less_than = (lhs%container < rhs%container)

      return
   end function __MANGLE(less_than)

   impure elemental function __MANGLE(greater_than_or_equal_to)(lhs, rhs) &
        & result(greater_than_or_equal_to)
      logical :: greater_than_or_equal_to
      type (__stack), intent(in) :: lhs
      type (__stack), intent(in) :: rhs

      greater_than_or_equal_to = .not. (lhs < rhs)

      return
   end function __MANGLE(greater_than_or_equal_to)


   impure elemental function __MANGLE(greater_than)(lhs, rhs) result(greater_than)
      logical :: greater_than
      type (__stack), intent(in) :: lhs
      type (__stack), intent(in) :: rhs

      greater_than = (rhs < lhs)

      return
   end function __MANGLE(greater_than)


   impure elemental function __MANGLE(less_than_or_equal_to)(lhs, rhs) &
        &  result(less_than_or_equal_to)
      logical :: less_than_or_equal_to
      type (__stack), intent(in) :: lhs
      type (__stack), intent(in) :: rhs

      less_than_or_equal_to = .not. (rhs < lhs)

      return
   end function __MANGLE(less_than_or_equal_to)


#endif


   ! =======================
   !  swap
   ! =======================
   subroutine __MANGLE(swap)(this, x)
      class(__stack), target, intent(inout) :: this
      type(__stack), target, intent(inout) :: x


      call this%container%swap(x%container)
      
      return
   end subroutine __MANGLE(swap)

   
   subroutine __MANGLE(write_formatted)(this, unit, iotype, v_list, iostat, iomsg)
    class(__stack), intent(in) :: this
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    iostat = 0

#ifdef __T_name__
    write(unit,'(a)') 'Stack<' // __T_declare_string__ // '>'
#else
    write(unit,'(a)') 'Stack<' // 'unknown' // '>'
#endif
    write(unit,'(a)') new_line('a')
    write(unit,'(4x,a10,1x,i0)') 'size: ',this%size()
   end subroutine __MANGLE(write_formatted)


! undef derived template parameters
#include "parameters/T/undef_derived_macros.inc"

! undef internal template parameters
#undef __guard
#include "parameters/T/undef_internal.inc"


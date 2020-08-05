!--------------------------------------------------------------------
! Copyright © 2020 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

      subroutine __MANGLE(iter_next)(this)
         class (__vector_iterator), intent(inout) :: this

         this%current_index = this%current_index + 1
      end subroutine __MANGLE(iter_next)


      subroutine __MANGLE(iter_prev)(this)
         class (__vector_iterator), intent(inout) :: this

         this%current_index = this%current_index - 1

      end subroutine __MANGLE(iter_prev)


      function __MANGLE(iter_at)(this) result(ptr)
         __T_declare_result__, pointer :: ptr
         class (__vector_iterator), intent(in) :: this

         ptr => this%elements(this%current_index)%item

      end function __MANGLE(iter_at)


      function __MANGLE(iter_at_offset_size_kind)(this, i) result(ptr)
         __T_declare_result__, pointer :: ptr
         class (__vector_iterator), intent(in) :: this
         integer(kind=__gftl_size_kind), intent(in) :: i

         ptr => this%elements(this%current_index + i)%item

      end function __MANGLE(iter_at_offset_size_kind)

      function __MANGLE(iter_at_offset_default)(this, i) result(ptr)
         __T_declare_result__, pointer :: ptr
         class (__vector_iterator), intent(in) :: this
         integer, intent(in) :: i

         ptr => this%elements(this%current_index + i)%item

      end function __MANGLE(iter_at_offset_default)


      subroutine __MANGLE(iter_add_n_size_kind)(this, n)
         class(__vector_iterator), intent(inout) :: this
         integer(kind=__gftl_size_kind), intent(in) :: n
         this%current_index = this%current_index + n
         return
      end subroutine __MANGLE(iter_add_n_size_kind)

      subroutine __MANGLE(iter_add_n_default)(this, n)
         class(__vector_iterator), intent(inout) :: this
         integer, intent(in) :: n
         this%current_index = this%current_index + n
         return
      end subroutine __MANGLE(iter_add_n_default)

      subroutine __MANGLE(iter_sub_n_size_kind)(this, n)
         class(__vector_iterator), intent(inout) :: this
         integer(kind=__gftl_size_kind), intent(in) :: n
         this%current_index = this%current_index - n
         return
      end subroutine __MANGLE(iter_sub_n_size_kind)

      subroutine __MANGLE(iter_sub_n_default)(this, n)
         class(__vector_iterator), intent(inout) :: this
         integer, intent(in) :: n
         this%current_index = this%current_index - n
         return
      end subroutine __MANGLE(iter_sub_n_default)


      ! Non type-bound procedures

      function __MANGLE(iter_a_plus_n_size_kind)(a, n) result(new_iter)
         type (__vector_iterator) :: new_iter
         type(__vector_iterator), intent(in) :: a
         integer(kind=__gftl_size_kind), intent(in) :: n

         new_iter = a
         call new_iter%add(n)

      end function __MANGLE(iter_a_plus_n_size_kind)

      function __MANGLE(iter_n_plus_a_size_kind)(n, a) result(new_iter)
         type (__vector_iterator) :: new_iter
         integer(kind=__gftl_size_kind), intent(in) :: n
         type(__vector_iterator), intent(in) :: a

         new_iter = a
         call new_iter%add(n)

      end function __MANGLE(iter_n_plus_a_size_kind)

      function __MANGLE(iter_a_minus_n_size_kind)(a, n) result(new_iter)
         type (__vector_iterator) :: new_iter
         type (__vector_iterator), intent(in) :: a
         integer(kind=__gftl_size_kind), intent(in) :: n

         new_iter = a
         call new_iter%sub(n)

      end function __MANGLE(iter_a_minus_n_size_kind)

      function __MANGLE(iter_a_plus_n_default)(a, n) result(new_iter)
         type (__vector_iterator) :: new_iter
         type (__vector_iterator), intent(in) :: a
         integer, intent(in) :: n

         new_iter = a
         call new_iter%add(n)

      end function __MANGLE(iter_a_plus_n_default)

      function __MANGLE(iter_n_plus_a_default)(n, a) result(new_iter)
         type (__vector_iterator) :: new_iter
         integer, intent(in) :: n
         type (__vector_iterator), intent(in) :: a

         new_iter = a
         call new_iter%add(n)

      end function __MANGLE(iter_n_plus_a_default)

      function __MANGLE(iter_a_minus_n_default)(a, n) result(new_iter)
         type (__vector_iterator) :: new_iter
         type (__vector_iterator), intent(in) :: a
         integer, intent(in) :: n

         new_iter = a
         call new_iter%sub(n)

      end function __MANGLE(iter_a_minus_n_default)

      function __MANGLE(iter_a_minus_b)(a, b) result(n)
         integer(kind=__gftl_size_kind) :: n
         type(__vector_iterator), intent(in) :: a
         type(__vector_iterator), intent(in) :: b

         n = a%current_index - b%current_index
      end function __MANGLE(iter_a_minus_b)


      ! Relational operators
      logical function __MANGLE(iter_equal)(a, b) result(eq)
         type(__vector_iterator), intent(in) :: a
         type(__vector_iterator), intent(in) :: b

         eq = (a%current_index == b%current_index)

      end function __MANGLE(iter_equal)


      logical function __MANGLE(iter_not_equal)(a, b) result(ne)
         type(__vector_iterator), intent(in) :: a
         type(__vector_iterator), intent(in) :: b

         ne = .not. (a == b)

      end function __MANGLE(iter_not_equal)

      logical function __MANGLE(iter_less)(a, b) result(less)
         type(__vector_iterator), intent(in) :: a
         type(__vector_iterator), intent(in) :: b
         less = (a%current_index < b%current_index)
      end function __MANGLE(iter_less)


      function __MANGLE(iter_less_equal)(a,b) result(le)
         logical :: le
         type(__vector_iterator), intent(in) :: a
         type(__vector_iterator), intent(in) :: b
         le = .not. (b < a)
      end function __MANGLE(iter_less_equal)

      logical function __MANGLE(iter_greater)(a, b) result(gt)
         type(__vector_iterator), intent(in) :: a
         type(__vector_iterator), intent(in) :: b
         gt = (a%current_index > b%current_index)
      end function __MANGLE(iter_greater)

      function __MANGLE(iter_greater_equal)(a,b) result(ge)
         logical :: ge
         type(__vector_iterator), intent(in) :: a
         type(__vector_iterator), intent(in) :: b
         ge = .not. (a < b)
      end function __MANGLE(iter_greater_equal)



      subroutine __MANGLE(iter_advance_size_kind)(it, n)
         type(__vector_iterator), intent(inout) :: it
         integer(kind=__gftl_size_kind), intent(in) :: n

         call it%add(n)

         return
      end subroutine __MANGLE(iter_advance_size_kind)

      subroutine __MANGLE(iter_advance_default)(it, n)
         type(__vector_iterator), intent(inout) :: it
         integer, intent(in) :: n

         call it%add(n)

         return
      end subroutine __MANGLE(iter_advance_default)


      function __MANGLE(iter_begin)(cont) result(begin)
         type(__vector_iterator) :: begin
         type(__vector), target, intent(in) :: cont

         begin = cont%begin()

         return
      end function __MANGLE(iter_begin)

      
      integer(kind=__gftl_size_kind) function __MANGLE(iter_distance)(a, b) result(distance)
         class(__vector_iterator), intent(in) :: a
         type(__vector_iterator), intent(in) :: b

         distance = b - a

         return
      end function __MANGLE(iter_distance)


      function __MANGLE(iter_end)(cont) result(end)
         type(__vector_iterator) :: end
         type(__vector), target, intent(in) :: cont

         end = cont%end()

         return
      end function __MANGLE(iter_end)


      function __MANGLE(iter_next_n_size_kind)(it, n) result(new_it)
         type(__vector_iterator) :: new_it
         type(__vector_iterator), intent(in) :: it
         integer(kind=__gftl_size_kind), intent(in) :: n

         new_it = it
         call new_it%add(n)

         return
      end function __MANGLE(iter_next_n_size_kind)

      function __MANGLE(iter_next_n_default)(it, n) result(new_it)
         type(__vector_iterator) :: new_it
         type(__vector_iterator), intent(in) :: it
         integer, intent(in) :: n

         new_it = it
         call new_it%add(n)

         return
      end function __MANGLE(iter_next_n_default)

      function __MANGLE(iter_prev_n_size_kind)(it, n) result(new_it)
         type(__vector_iterator) :: new_it
         type(__vector_iterator), intent(in) :: it
         integer(kind=__gftl_size_kind), intent(in) :: n

         new_it = it
         call new_it%sub(n)

         return
      end function __MANGLE(iter_prev_n_size_kind)

      function __MANGLE(iter_prev_n_default)(it, n) result(new_it)
         type(__vector_iterator) :: new_it
         type(__vector_iterator), intent(in) :: it
         integer, intent(in) :: n

         new_it = it
         call new_it%sub(n)

         return
      end function __MANGLE(iter_prev_n_default)



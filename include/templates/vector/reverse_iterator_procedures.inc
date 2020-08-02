!--------------------------------------------------------------------
! Copyright © 2020 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

      function __MANGLE(riter_get)(this) result (item)
         class (__vector_riterator), intent(in) :: this

         __T_declare_result__, pointer :: item
         item => this%elements(this%current_index)%item

      end function __MANGLE(riter_get)


      subroutine __MANGLE(riter_next)(this)
         class (__vector_riterator), intent(inout) :: this

         this%current_index = this%current_index - 1
      end subroutine __MANGLE(riter_next)


      subroutine __MANGLE(riter_previous)(this)
         class (__vector_riterator), intent(inout) :: this

         this%current_index = this%current_index + 1

      end subroutine __MANGLE(riter_previous)



      function __MANGLE(riter_at)(this) result(ptr)
         __T_declare_result__, pointer :: ptr
         class (__vector_riterator), intent(in) :: this

         ptr => this%elements(this%current_index)%item

      end function __MANGLE(riter_at)


      function __MANGLE(riter_at_offset_size_kind)(this, i) result(ptr)
         __T_declare_result__, pointer :: ptr
         class (__vector_riterator), intent(in) :: this
         integer(kind=__gftl_size_kind), intent(in) :: i

         ptr => this%elements(this%current_index + i)%item

      end function __MANGLE(riter_at_offset_size_kind)

      function __MANGLE(riter_at_offset_default)(this, i) result(ptr)
         __T_declare_result__, pointer :: ptr
         class (__vector_riterator), intent(in) :: this
         integer, intent(in) :: i

         ptr => this%elements(this%current_index + i)%item
         
      end function __MANGLE(riter_at_offset_default)



      subroutine __MANGLE(riter_add_n_size_kind)(this, n)
         class(__vector_riterator), intent(inout) :: this
         integer(kind=__gftl_size_kind), intent(in) :: n
         this%current_index = this%current_index - n
         return
      end subroutine __MANGLE(riter_add_n_size_kind)

      subroutine __MANGLE(riter_add_n_default)(this, n)
         class(__vector_riterator), intent(inout) :: this
         integer, intent(in) :: n
         this%current_index = this%current_index - n
         return
      end subroutine __MANGLE(riter_add_n_default)

      subroutine __MANGLE(riter_sub_n_size_kind)(this, n)
         class(__vector_riterator), intent(inout) :: this
         integer(kind=__gftl_size_kind), intent(in) :: n
         this%current_index = this%current_index + n
         return
      end subroutine __MANGLE(riter_sub_n_size_kind)

      subroutine __MANGLE(riter_sub_n_default)(this, n)
         class(__vector_riterator), intent(inout) :: this
         integer, intent(in) :: n
         this%current_index = this%current_index + n
         return
      end subroutine __MANGLE(riter_sub_n_default)




      function __MANGLE(riter_a_plus_n_size_kind)(a, n) result(new_iter)
         type (__vector_riterator) :: new_iter
         type(__vector_riterator), intent(in) :: a
         integer(kind=__gftl_size_kind), intent(in) :: n

         new_iter = a
         call new_iter%add_n(n)

      end function __MANGLE(riter_a_plus_n_size_kind)

      function __MANGLE(riter_n_plus_a_size_kind)(n, a) result(new_iter)
         type (__vector_riterator) :: new_iter
         integer(kind=__gftl_size_kind), intent(in) :: n
         type(__vector_riterator), intent(in) :: a

         new_iter = a
         call new_iter%add_n(n)

      end function __MANGLE(riter_n_plus_a_size_kind)

      function __MANGLE(riter_a_minus_n_size_kind)(a, n) result(new_iter)
         type (__vector_riterator) :: new_iter
         type (__vector_riterator), intent(in) :: a
         integer(kind=__gftl_size_kind), intent(in) :: n

         new_iter = a
         call new_iter%sub_n(n)

      end function __MANGLE(riter_a_minus_n_size_kind)

      function __MANGLE(riter_a_plus_n_default)(a, n) result(new_iter)
         type (__vector_riterator) :: new_iter
         type (__vector_riterator), intent(in) :: a
         integer, intent(in) :: n

         new_iter = a
         call new_iter%add_n(n)

      end function __MANGLE(riter_a_plus_n_default)

      function __MANGLE(riter_n_plus_a_default)(n, a) result(new_iter)
         type (__vector_riterator) :: new_iter
         integer, intent(in) :: n
         type (__vector_riterator), intent(in) :: a

         new_iter = a
         call new_iter%add_n(n)

      end function __MANGLE(riter_n_plus_a_default)

      function __MANGLE(riter_a_minus_n_default)(a, n) result(new_iter)
         type (__vector_riterator) :: new_iter
         type (__vector_riterator), intent(in) :: a
         integer, intent(in) :: n

         new_iter = a
         call new_iter%sub_n(n)

      end function __MANGLE(riter_a_minus_n_default)

      function __MANGLE(riter_a_minus_b)(a, b) result(n)
         integer(kind=__gftl_size_kind) :: n
         type(__vector_riterator), intent(in) :: a
         type(__vector_riterator), intent(in) :: b

         n = a%current_index - b%current_index
      end function __MANGLE(riter_a_minus_b)




      ! Relational operators
      logical function __MANGLE(riter_equal)(a, b) result(eq)
         type(__vector_riterator), intent(in) :: a
         type(__vector_riterator), intent(in) :: b

         eq = (a%current_index == b%current_index)

      end function __MANGLE(riter_equal)


      logical function __MANGLE(riter_not_equal)(a, b) result(ne)
         type(__vector_riterator), intent(in) :: a
         type(__vector_riterator), intent(in) :: b

         ne = .not. (a == b)

      end function __MANGLE(riter_not_equal)

      logical function __MANGLE(riter_less)(a, b) result(less)
         type(__vector_riterator), intent(in) :: a
         type(__vector_riterator), intent(in) :: b
         less = (a%current_index < b%current_index)
      end function __MANGLE(riter_less)


      function __MANGLE(riter_less_equal)(a,b) result(le)
         logical :: le
         type(__vector_riterator), intent(in) :: a
         type(__vector_riterator), intent(in) :: b
         le = .not. (b < a)
      end function __MANGLE(riter_less_equal)

      logical function __MANGLE(riter_greater)(a, b) result(gt)
         type(__vector_riterator), intent(in) :: a
         type(__vector_riterator), intent(in) :: b
         gt = (b < a)
      end function __MANGLE(riter_greater)

      function __MANGLE(riter_greater_equal)(a,b) result(ge)
         logical :: ge
         type(__vector_riterator), intent(in) :: a
         type(__vector_riterator), intent(in) :: b
         ge = .not. (a < b)

      end function __MANGLE(riter_greater_equal)




      subroutine __MANGLE(riter_advance_size_kind)(it, n)
         type(__vector_riterator), intent(inout) :: it
         integer(kind=__gftl_size_kind), intent(in) :: n

         call it%add_n(n)

         return
      end subroutine __MANGLE(riter_advance_size_kind)

      subroutine __MANGLE(riter_advance_default)(it, n)
         type(__vector_riterator), intent(inout) :: it
         integer, intent(in) :: n

         call it%add_n(n)

         return
      end subroutine __MANGLE(riter_advance_default)


      integer(kind=__gftl_size_kind) function __MANGLE(riter_distance)(a, b) result(distance)
         class(__vector_riterator), intent(in) :: a
         type(__vector_riterator), intent(in) :: b

         distance = b - a

         return
      end function __MANGLE(riter_distance)


      function __MANGLE(riter_next_f)(it) result(new_it)
         type(__vector_riterator) :: new_it
         type(__vector_riterator), intent(in) :: it

         new_it = it
         call new_it%next()
         
      end function __MANGLE(riter_next_f)

      function __MANGLE(riter_previous_f)(it) result(new_it)
         type(__vector_riterator) :: new_it
         type(__vector_riterator), intent(in) :: it

         new_it = it
         call new_it%previous()
         
      end function __MANGLE(riter_previous_f)


      

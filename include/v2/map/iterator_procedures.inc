! =======================
!  Pair: of()
! =======================
     function __MANGLE(iter_of)(this) result(p)
        type(__map_pair), pointer :: p
         class(__map_iterator), target, intent(in) :: this

         p => this%set_iter%of()

      end function __MANGLE(iter_of)

! =======================
!  key: first() 
! =======================
      function __MANGLE(iter_first)(this) result(first)
         __Key_declare_result__, pointer :: first
         class(__map_iterator), target, intent(in) :: this

         type(__map_pair), pointer :: p

         p => this%of()
         if (associated(p)) then
            first => p%first
         else
            first => null()
         end if

      end function __MANGLE(iter_first)

! =======================
!  value: second()
! =======================
      function __MANGLE(iter_second)(this) result(second)
         __T_declare_result__, pointer :: second
         class(__map_iterator), target, intent(in) :: this

         type(__map_pair), pointer :: p

         p => this%of()
         if (associated(p)) then
            second => p%second
         else
            second => null()
         end if

      end function __MANGLE(iter_second)



! =======================
!  operator(==)
! =======================
      logical function __MANGLE(iter_equal)(a, b) result(equal)
         type(__map_iterator), intent(in) :: a
         type(__map_iterator), intent(in) :: b

         equal = (a%set_iter == b%set_iter)

      end function __MANGLE(iter_equal)


! =======================
!  operator(/=)
! =======================
      logical function __MANGLE(iter_not_equal)(a, b) result(not_equal)
         type(__map_iterator), intent(in) :: a
         type(__map_iterator), intent(in) :: b

         not_equal = .not. (a == b)
      end function __MANGLE(iter_not_equal)


! =======================
!  next
! =======================
      subroutine __MANGLE(iter_next)(this)
         class(__map_iterator), intent(inout) :: this

         call this%set_iter%next()
      end subroutine __MANGLE(iter_next)


! =======================
!  prev
! =======================
      subroutine __MANGLE(iter_prev)(this)
         class(__map_iterator), intent(inout) :: this

         call this%set_iter%prev()
      end subroutine __MANGLE(iter_prev)



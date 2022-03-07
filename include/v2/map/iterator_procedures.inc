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

! ===============================
! NON-type bound module procedure
! ===============================

! =========
! advance
! =========

      subroutine __MANGLE(iter_advance_size_kind)(it, n)
         type(__map_iterator), intent(inout) :: it
         integer(kind=__gftl_size_kind), intent(in) :: n

         integer :: i

         do i = 1, n
            call it%next()
         end do
         return
      end subroutine __MANGLE(iter_advance_size_kind)

      subroutine __MANGLE(iter_advance_default)(it, n)
         type(__map_iterator), intent(inout) :: it
         integer, intent(in) :: n
         integer :: i

         do i = 1, n
            call it%next()
         end do

         return
      end subroutine __MANGLE(iter_advance_default)

! ======
! begin
! ======

      function __MANGLE(iter_begin)(cont) result(begin)
         type(__map_iterator) :: begin
         type(__map), target, intent(in) :: cont

         begin = cont%begin()

         return
      end function __MANGLE(iter_begin)

! =====
! end
! =====

      function __MANGLE(iter_end)(cont) result(end)
         type(__map_iterator) :: end
         type(__map), target, intent(in) :: cont

         end = cont%end()

         return
      end function __MANGLE(iter_end)

! ======
! next
! ======

      function __MANGLE(iter_next_1)(it) result(new_it)
         type(__map_iterator) :: new_it
         type(__map_iterator), intent(in) :: it

         new_it = next(it,1)

         return
      end function __MANGLE(iter_next_1)

      function __MANGLE(iter_next_n_size_kind)(it, n) result(new_it)
         type(__map_iterator) :: new_it
         type(__map_iterator), intent(in) :: it
         integer(kind=__gftl_size_kind), intent(in) :: n
         integer :: i

         new_it = it

         do i = 1, n
            call new_it%next()
         end do

         return
      end function __MANGLE(iter_next_n_size_kind)

      function __MANGLE(iter_next_n_default)(it, n) result(new_it)
         type(__map_iterator) :: new_it
         type(__map_iterator), intent(in) :: it
         integer, intent(in) :: n
         integer :: i

         new_it = it

         do i = 1, n
            call new_it%next()
         end do

         return
      end function __MANGLE(iter_next_n_default)

! =====
! prev
! =====

     function __MANGLE(iter_prev_1)(it) result(new_it)
         type(__map_iterator) :: new_it
         type(__map_iterator), intent(in) :: it

         new_it = prev(it,1)

         return
      end function __MANGLE(iter_prev_1)

      function __MANGLE(iter_prev_n_size_kind)(it, n) result(new_it)
         type(__map_iterator) :: new_it
         type(__map_iterator), intent(in) :: it
         integer(kind=__gftl_size_kind), intent(in) :: n
         integer :: i

         new_it = it

         do i = 1, n
            call new_it%prev()
         enddo

         return
      end function __MANGLE(iter_prev_n_size_kind)

      function __MANGLE(iter_prev_n_default)(it, n) result(new_it)
         type(__map_iterator) :: new_it
         type(__map_iterator), intent(in) :: it
         integer, intent(in) :: n
         integer :: i

         new_it = it

         do i = 1, n
            call new_it%prev()
         enddo

         return
      end function __MANGLE(iter_prev_n_default)


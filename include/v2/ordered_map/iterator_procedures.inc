! =======================
!  Pair: at()
! =======================
     function __MANGLE(iter_at)(this) result(p)
        type(__omap_pair), pointer :: p
         class(__omap_iterator), target, intent(in) :: this

         type(__MANGLE(MapIterator)) :: map_iter
         __Key_declare_result__, pointer :: key

         if (this%key_iter /= this%reference%ordered_keys%end()) then
            key => this%key_iter%of()
            if (associated(key)) then
               map_iter = this%reference%map%find(key)
               p => map_iter%of()
            else
               p => null()
            end if
         else
            p => null()
         end if

      end function __MANGLE(iter_at)

! =======================
!  key: first() 
! =======================
      function __MANGLE(iter_first)(this) result(first)
         __Key_declare_result__, pointer :: first
         class(__omap_iterator), target, intent(in) :: this

         type(__omap_pair), pointer :: p

         p => this%at()
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
         class(__omap_iterator), target, intent(in) :: this

         type(__omap_pair), pointer :: p

         p => this%at()
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
         type(__omap_iterator), intent(in) :: a
         type(__omap_iterator), intent(in) :: b

         equal = (a%key_iter == b%key_iter)

      end function __MANGLE(iter_equal)


! =======================
!  operator(/=)
! =======================
      logical function __MANGLE(iter_not_equal)(a, b) result(not_equal)
         type(__omap_iterator), intent(in) :: a
         type(__omap_iterator), intent(in) :: b

         not_equal = .not. (a == b)
      end function __MANGLE(iter_not_equal)


! =======================
!  next
! =======================
      subroutine __MANGLE(iter_next)(this)
         class(__omap_iterator), intent(inout) :: this

         call this%key_iter%next()
      end subroutine __MANGLE(iter_next)


! =======================
!  prev
! =======================
      subroutine __MANGLE(iter_prev)(this)
         class(__omap_iterator), intent(inout) :: this

         call this%key_iter%prev()
      end subroutine __MANGLE(iter_prev)

! ===============================
! NON-type bound module procedure
! ===============================

! =========
! advance
! =========

      subroutine __MANGLE(iter_advance_size_kind)(it, n)
         type(__omap_iterator), intent(inout) :: it
         integer(kind=__gftl_size_kind), intent(in) :: n
         integer :: i

         do i = 1, n
            call it%next()
         end do

         return
      end subroutine __MANGLE(iter_advance_size_kind)

      subroutine __MANGLE(iter_advance_default)(it, n)
         type(__omap_iterator), intent(inout) :: it
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
         type(__omap_iterator) :: begin
         type(__omap), target, intent(in) :: cont

         begin = cont%begin()

         return
      end function __MANGLE(iter_begin)

! =====
! end
! =====

      function __MANGLE(iter_end)(cont) result(end)
         type(__omap_iterator) :: end
         type(__omap), target, intent(in) :: cont

         end = cont%end()

         return
      end function __MANGLE(iter_end)

! ======
! ftn_begin
! ======

      function __MANGLE(iter_ftn_begin)(cont) result(begin)
         type(__omap_iterator) :: begin
         type(__omap), target, intent(in) :: cont

         begin = cont%ftn_begin()

         return
      end function __MANGLE(iter_ftn_begin)

! =====
! ftn_end
! =====

      function __MANGLE(iter_ftn_end)(cont) result(end)
         type(__omap_iterator) :: end
         type(__omap), target, intent(in) :: cont

         end = cont%ftn_end()

         return
      end function __MANGLE(iter_ftn_end)

! ======
! next
! ======

      function __MANGLE(iter_next_1)(it) result(new_it)
         type(__omap_iterator) :: new_it
         type(__omap_iterator), intent(in) :: it

         new_it = next(it,1)

         return
      end function __MANGLE(iter_next_1)

      function __MANGLE(iter_next_n_size_kind)(it, n) result(new_it)
         type(__omap_iterator) :: new_it
         type(__omap_iterator), intent(in) :: it
         integer(kind=__gftl_size_kind), intent(in) :: n
         integer :: i

         new_it = it
         
         do i = 1, n
            call new_it%next()
         end do

         return
      end function __MANGLE(iter_next_n_size_kind)

      function __MANGLE(iter_next_n_default)(it, n) result(new_it)
         type(__omap_iterator) :: new_it
         type(__omap_iterator), intent(in) :: it
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
         type(__omap_iterator) :: new_it
         type(__omap_iterator), intent(in) :: it

         new_it = prev(it,1)

         return
      end function __MANGLE(iter_prev_1)

      function __MANGLE(iter_prev_n_size_kind)(it, n) result(new_it)
         type(__omap_iterator) :: new_it
         type(__omap_iterator), intent(in) :: it
         integer(kind=__gftl_size_kind), intent(in) :: n
         integer :: i

         new_it = it
         do i = 1, n
            call new_it%prev()
         end do

         return
      end function __MANGLE(iter_prev_n_size_kind)

      function __MANGLE(iter_prev_n_default)(it, n) result(new_it)
         type(__omap_iterator) :: new_it
         type(__omap_iterator), intent(in) :: it
         integer, intent(in) :: n
         integer :: i

         new_it = it

         do i = 1, n
            call new_it%prev()
         end do

         return
      end function __MANGLE(iter_prev_n_default)

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



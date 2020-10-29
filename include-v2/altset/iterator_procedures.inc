   ! =======================
   !  value
   ! =======================
   function __MANGLE(iter_at)(this) result(at)
      __T_declare_result__, pointer :: at
      class(__set_iterator), target, intent(in) :: this

      if (this%current == __UNINITIALIZED) then
         at => null()
      else
         at => this%reference%items%at(this%current)
      end if

   end function __MANGLE(iter_at)

   ! =======================
   !  next
   ! =======================
   subroutine __MANGLE(iter_next)(this)
      class(__set_iterator), intent(inout) :: this

      call this%reference%advpos(this%current, 1)

   end subroutine __MANGLE(iter_next)

   ! =======================
   !  prev
   ! =======================
   subroutine __MANGLE(iter_prev)(this)
      class(__set_iterator), intent(inout) :: this

      call this%reference%advpos(this%current,0)

   end subroutine __MANGLE(iter_prev)

   ! =======================
   !  equalIters
   ! =======================
   logical function __MANGLE(iter_equal)(a, b) result(eq)
      type(__set_iterator), intent(in) :: a, b
      
      eq = a%current == b%current

   end function __MANGLE(iter_equal)

   ! =======================
   !  nequal
   ! =======================
   logical function __MANGLE(iter_not_equal)(a, b) result(ne)
      implicit none
      type(__set_iterator), intent(in) :: a, b

      ne = .not. (a == b)

   end function __MANGLE(iter_not_equal)



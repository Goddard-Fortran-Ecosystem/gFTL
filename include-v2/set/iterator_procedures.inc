   ! =======================
   !  value
   ! =======================
   function __MANGLE(iter_at)(this) result(value)
      class(__set_iterator), intent(in) :: this
      __T_declare_result__, pointer :: value

      if (associated(this%node)) then
         value  => this%node%value
      else
         value => null()
      end if

   end function __MANGLE(iter_at)

   ! =======================
   !  next
   ! =======================
   subroutine __MANGLE(iter_next)(this)
      class(__set_iterator), intent(inout) :: this

      call this%tree%advpos(this%node, __RIGHT)

   end subroutine __MANGLE(iter_next)

   ! =======================
   !  prev
   ! =======================
   subroutine __MANGLE(iter_prev)(this)
      class(__set_iterator), intent(inout) :: this

      call this%tree%advpos(this%node, __LEFT)

   end subroutine __MANGLE(iter_prev)

   ! =======================
   !  equalIters
   ! =======================
   logical function __MANGLE(iter_equal)(a, b) result(eq)
      type(__set_iterator), intent(in) :: a
      type(__set_iterator), intent(in) :: b
      
      eq = &
           &    associated(a%tree, target=b%tree) .and.                         &
           &   ((.not.associated(a%node) .and. .not.associated(b%node))         &
           &   .or.associated(a%node, target=b%node))

   end function __MANGLE(iter_equal)

   ! =======================
   !  nequal
   ! =======================
   logical function __MANGLE(iter_not_equal)(a, b) result(ne)
      implicit none
      class(__set_iterator), intent(in) :: a, b

      ne = .not. (a == b)

   end function __MANGLE(iter_not_equal)





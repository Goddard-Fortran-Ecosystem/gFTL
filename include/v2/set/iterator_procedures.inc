   ! =======================
   !  value
   ! =======================
   function __MANGLE(iter_of)(this) result(value)
      class(__set_iterator), intent(in) :: this
      __T_declare_result__, pointer :: value

      if (associated(this%node)) then
         value  => this%node%value
      else
         value => null()
      end if

   end function __MANGLE(iter_of)

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


   subroutine __MANGLE(iter_advance_size_kind)(it, n)
      type(__set_iterator), intent(inout) :: it
      integer(kind=__gftl_size_kind), intent(in) :: n

      integer :: i

      do i = 1, n
         call it%next()
      end do
      
      return
   end subroutine __MANGLE(iter_advance_size_kind)
   
   subroutine __MANGLE(iter_advance_default)(it, n)
      type(__set_iterator), intent(inout) :: it
      integer, intent(in) :: n
      
      integer :: i

      do i = 1, n
         call it%next()
      end do
      
      return
   end subroutine __MANGLE(iter_advance_default)
   
   
   function __MANGLE(iter_begin)(cont) result(begin)
      type(__set_iterator) :: begin
      type(__set), target, intent(in) :: cont
      
      begin = cont%begin()
      
      return
   end function __MANGLE(iter_begin)
   
   
   
   function __MANGLE(iter_end)(cont) result(end)
      type(__set_iterator) :: end
      type(__set), target, intent(in) :: cont

      end = cont%end()
      
   end function __MANGLE(iter_end)
   

   function __MANGLE(iter_next_1)(it) result(new_it)
      type(__set_iterator) :: new_it
      type(__set_iterator), intent(in) :: it
      
      new_it = next(it,1)
      
      return
   end function __MANGLE(iter_next_1)
   
   function __MANGLE(iter_next_n_size_kind)(it, n) result(new_it)
      type(__set_iterator) :: new_it
      type(__set_iterator), intent(in) :: it
      integer(kind=__gftl_size_kind), intent(in) :: n
      
      integer :: i

      new_it = it
      do i = 1, n
         call new_it%next()
      end do
      
      return
   end function __MANGLE(iter_next_n_size_kind)
   
   function __MANGLE(iter_next_n_default)(it, n) result(new_it)
      type(__set_iterator) :: new_it
      type(__set_iterator), intent(in) :: it
      integer, intent(in) :: n
      
      integer :: i

      new_it = it
      do i = 1, n
         call new_it%next()
      end do
      
      return
   end function __MANGLE(iter_next_n_default)
   
   function __MANGLE(iter_prev_1)(it) result(new_it)
      type(__set_iterator) :: new_it
      type(__set_iterator), intent(in) :: it
      
      new_it = prev(it,1)
      
      return
   end function __MANGLE(iter_prev_1)
   
   function __MANGLE(iter_prev_n_size_kind)(it, n) result(new_it)
      type(__set_iterator) :: new_it
      type(__set_iterator), intent(in) :: it
      integer(kind=__gftl_size_kind), intent(in) :: n
      
      integer :: i

      new_it = it
      do i = 1, n
         call new_it%prev()
      end do
      
      return
   end function __MANGLE(iter_prev_n_size_kind)
   
   function __MANGLE(iter_prev_n_default)(it, n) result(new_it)
      type(__set_iterator) :: new_it
      type(__set_iterator), intent(in) :: it
      integer, intent(in) :: n
      
      integer :: i

      new_it = it
      do i = 1, n
         call new_it%prev()
      end do
         
      return
   end function __MANGLE(iter_prev_n_default)
   

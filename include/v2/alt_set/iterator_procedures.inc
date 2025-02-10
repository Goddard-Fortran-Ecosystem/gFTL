   ! =======================
   !  iter_of
   ! =======================
   function __MANGLE(iter_of)(this) result(value)
      class(__alt_set_iterator), intent(in) :: this
      __T_declare_result__, pointer :: value

      value => null()
      if (__MANGLE(exists)(this%current)) then
         value  => this%tree%items%of(this%current)
      end if

   end function __MANGLE(iter_of)

   ! =======================
   !  iter_next
   ! =======================
   subroutine __MANGLE(iter_next)(this)
      class(__alt_set_iterator), intent(inout) :: this

      call this%tree%advpos(this%current, __RIGHT)

   end subroutine __MANGLE(iter_next)

   ! =======================
   !  iter_prev
   ! =======================
   subroutine __MANGLE(iter_prev)(this)
      class(__alt_set_iterator), intent(inout) :: this

      call this%tree%advpos(this%current, __LEFT)

   end subroutine __MANGLE(iter_prev)

   ! =======================
   !  equalIters
   ! =======================
   logical function __MANGLE(iter_equal)(a, b) result(eq)
      type(__alt_set_iterator), intent(in) :: a
      type(__alt_set_iterator), intent(in) :: b

      eq = (a%current == b%current)

   end function __MANGLE(iter_equal)

   ! =======================
   !  nequal
   ! =======================
   logical function __MANGLE(iter_not_equal)(a, b) result(ne)
      implicit none
      class(__alt_set_iterator), intent(in) :: a, b

      ne = .not. (a == b)

   end function __MANGLE(iter_not_equal)


   subroutine __MANGLE(iter_advance_size_kind)(it, n)
      type(__alt_set_iterator), intent(inout) :: it
      integer(kind=__gftl_size_kind), intent(in) :: n

      integer :: i

      do i = 1, n
         call it%next()
      end do
      
      return
   end subroutine __MANGLE(iter_advance_size_kind)
   
   subroutine __MANGLE(iter_advance_default)(it, n)
      type(__alt_set_iterator), intent(inout) :: it
      integer, intent(in) :: n
      
      integer :: i

      do i = 1, n
         call it%next()
      end do
      
      return
   end subroutine __MANGLE(iter_advance_default)
   
   
   function __MANGLE(iter_begin)(cont) result(begin)
      type(__alt_set_iterator) :: begin
      type(__alt_set), target, intent(in) :: cont
      
      begin = cont%begin()
      
      return
   end function __MANGLE(iter_begin)
   
   
   
   function __MANGLE(iter_end)(cont) result(end)
      type(__alt_set_iterator) :: end
      type(__alt_set), target, intent(in) :: cont

      end = cont%end()
      
   end function __MANGLE(iter_end)
   

   function __MANGLE(iter_ftn_begin)(cont) result(begin)
      type(__alt_set_iterator) :: begin
      type(__alt_set), target, intent(in) :: cont
      
      begin = cont%ftn_begin()
      
      return
   end function __MANGLE(iter_ftn_begin)
   
   
   
   function __MANGLE(iter_ftn_end)(cont) result(end)
      type(__alt_set_iterator) :: end
      type(__alt_set), target, intent(in) :: cont

      end = cont%ftn_end()
      
   end function __MANGLE(iter_ftn_end)
   

   function __MANGLE(iter_next_1)(it) result(new_it)
      type(__alt_set_iterator) :: new_it
      type(__alt_set_iterator), intent(in) :: it
      
      new_it = next(it,1)
      
      return
   end function __MANGLE(iter_next_1)
   
   function __MANGLE(iter_next_n_size_kind)(it, n) result(new_it)
      type(__alt_set_iterator) :: new_it
      type(__alt_set_iterator), intent(in) :: it
      integer(kind=__gftl_size_kind), intent(in) :: n
      
      integer :: i

      new_it = it
      do i = 1, n
         call new_it%next()
      end do
      
      return
   end function __MANGLE(iter_next_n_size_kind)
   
   function __MANGLE(iter_next_n_default)(it, n) result(new_it)
      type(__alt_set_iterator) :: new_it
      type(__alt_set_iterator), intent(in) :: it
      integer, intent(in) :: n
      
      integer :: i

      new_it = it
      do i = 1, n
         call new_it%next()
      end do
      
      return
   end function __MANGLE(iter_next_n_default)
   
   function __MANGLE(iter_prev_1)(it) result(new_it)
      type(__alt_set_iterator) :: new_it
      type(__alt_set_iterator), intent(in) :: it
      
      new_it = prev(it,1)
      
      return
   end function __MANGLE(iter_prev_1)
   
   function __MANGLE(iter_prev_n_size_kind)(it, n) result(new_it)
      type(__alt_set_iterator) :: new_it
      type(__alt_set_iterator), intent(in) :: it
      integer(kind=__gftl_size_kind), intent(in) :: n
      
      integer :: i

      new_it = it
      do i = 1, n
         call new_it%prev()
      end do
      
      return
   end function __MANGLE(iter_prev_n_size_kind)
   
   function __MANGLE(iter_prev_n_default)(it, n) result(new_it)
      type(__alt_set_iterator) :: new_it
      type(__alt_set_iterator), intent(in) :: it
      integer, intent(in) :: n
      
      integer :: i

      new_it = it
      do i = 1, n
         call new_it%prev()
      end do
         
      return
   end function __MANGLE(iter_prev_n_default)
   

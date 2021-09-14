
   ! =======================
   !  get_parent()
   ! =======================
   function __MANGLE(get_parent)(this) result(parent)
      class(__set_node), intent(in) :: this
      type(__set_node), pointer :: parent

      parent => this%parent

   end function __MANGLE(get_parent)


   ! =======================
   !  has_child()
   ! =======================
   logical function __MANGLE(has_child)(this, side) result(has_child)
      class(__set_node), intent(in) :: this
      integer, intent(in) :: side

      if (side ==__LEFT) has_child = allocated(this%left)
      if (side == __RIGHT) has_child = allocated(this%right)

   end function __MANGLE(has_child)
   
   ! =======================
   !  get_child()
   ! =======================
   function __MANGLE(get_child)(this, side) result(child)
      type(__set_node), pointer :: child
      class(__set_node), target, intent(in) :: this
      integer, intent(in) :: side

      if (side == __LEFT) then
         select type (q => this%left)
         type is (__set_node)
            child => q
         end select
         return
      end if

      if (side == __RIGHT) then
         select type (q => this%right)
         type is (__set_node)
            child => q
         end select
      end if

   end function __MANGLE(get_child)

   ! =======================
   !  which_side_am_i()
   ! =======================
   integer function __MANGLE(which_side_am_i)(this) result(side)
      class(__set_node), target, intent(in) :: this

      type(__set_node), pointer :: parent

      parent => this%get_parent()
      if (.not. associated(parent)) error stop 'root node is neither left nor right'

      side = parent%which_child(this)
      
   end function __MANGLE(which_side_am_i)

   ! =======================
   !  which_child()
   ! =======================
   function __MANGLE(which_child)(this, child) result(side)
      integer :: side
      class(__set_node), intent(in) :: this
      type(__set_node), target, intent(in) :: child

      type(__set_node), pointer :: left

      left => this%get_child(__LEFT)
      if (associated(left)) then
         if (associated(left, target=child)) then
            side = __LEFT
            return
         end if
      else
         side = __RIGHT
      end if
      return

   end function __MANGLE(which_child)

   ! =======================
   !  set_child()
   ! =======================
   subroutine __MANGLE(set_child)(this, side, child)
      class(__set_node), intent(inout) :: this
      integer, intent(in) :: side
      type(__set_node), pointer, intent(in) :: child

      if (side == __LEFT)  allocate(this%left, source=child)
      if (side == __RIGHT) allocate(this%right, source=child)
      return
      
   end subroutine __MANGLE(set_child)

   ! =======================
   !  get_height()
   ! =======================
   integer function __MANGLE(get_height)(this) result(height)
      class(__set_node), intent(in) :: this
      height = this%height
   end function __MANGLE(get_height)

   ! =======================
   !  update_height()
   ! =======================
   subroutine __MANGLE(update_height)(this)
      class(__set_node), intent(inout) :: this
      integer :: h0, h1

      h0 = 0
      h1 = 0
      if (allocated(this%left)) h0 = this%left%get_height()
      if (allocated(this%right)) h1 = this%right%get_height()
      this%height = max(h0, h1) + 1

      return
   end subroutine __MANGLE(update_height)



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
   logical function __MANGLE(has_child)(this, side)
      class(__set_node), target, intent(in) :: this
      integer :: side
      type(__set_node), pointer :: child

      if (side ==__LEFT) has_child = allocate(this%left)
      if (side == __RIGHT) has_child = allocated(this%right)

   end function __MANGLE(has_child)
   
   ! =======================
   !  get_child()
   ! =======================
   function __MANGLE(get_child)(this, side) result(child)
      class(__set_node), target, intent(in) :: this
      integer :: idx
      type(__set_node), pointer :: child

      if (side == __LEFT) then
         select type (q => this%left)
         type is (__set_node)
            child => q
         end select
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
   integer function which_side_am_i(this) result(side)
      class(__set_node), target, intent(in) :: this

      type(__set_node), pointer :: parent

      parent => this%get_parent
      if (.not. associated(parent)) error stop 'root node is neither left nor right'

      side = parent%which_child(this)
      
   end function I_which_side_am_i

   ! =======================
   !  which_child()
   ! =======================
   function __MANGLE(which_child)(this, child) result(side)
      integer :: side
      class(__set_node), intent(in) :: this
      type(__set_node), target, intent(in) :: child

      side = merge(__LEFT, __RIGHT, associated(this%left, target=child))

   end function __MANGLE(which_child)

   ! =======================
   !  set_child()
   ! =======================
   subroutine __MANGLE(set_child)(this, idx, child)
      class(__set_node), intent(inout) :: this
      integer, intent(in) :: idx
      type(__set_node), pointer, intent(in) :: child

      if (idx==__LEFT) this%left => child
      if (idx==__RIGHT) this%right => child
      return
   end subroutine __MANGLE(set_child)

   ! =======================
   !  update_height()
   ! =======================
   subroutine __MANGLE(update_height)(this)
      class(__set_node), target, intent(inout) :: this
      integer :: h0, h1

      h0 = 0
      h1 = 0
      if (allocated(this%left)) h0 = this%left%height
      if (allocated(this%right)) h1 = this%right%height
      this%height = max(h0, h1) + 1

      return
   end subroutine __MANGLE(update_height)



   ! =======================
   !  child()
   ! =======================
   function __MANGLE(child)(this, idx) result(child)
      class(__set_node), target, intent(in) :: this
      integer :: idx
      type(__set_node), pointer :: child

      if (idx==0) child=>this%left
      if (idx==1) child=>this%right

   end function __MANGLE(child)

   ! =======================
   !  which_child()
   ! =======================
   function __MANGLE(which_child)(this, child) result(which_child)
      class(__set_node), intent(in) :: this
      type(__set_node), target, intent(in) :: child
      integer :: which_child

      which_child = merge(__LEFT, __RIGHT, associated(this%left, target=child))

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
      if (associated(this%left)) h0 = this%left%height
      if (associated(this%right)) h1 = this%right%height
      this%height = max(h0, h1) + 1

      return
   end subroutine __MANGLE(update_height)

   
   recursive subroutine __MANGLE(finalize_node)(this)
      type(__set_node), intent(inout) :: this
      
      print*,'final: set', this%value
      if (associated(this%left)) deallocate(this%left)
      if (associated(this%right)) deallocate(this%right)

      return
   end subroutine __MANGLE(finalize_node)

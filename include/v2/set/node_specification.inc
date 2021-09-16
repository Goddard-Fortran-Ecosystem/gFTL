   type, abstract :: __base_node
   contains
      procedure(I_to_node), deferred :: to_node
      procedure(I_get_parent), deferred :: get_parent
      procedure(I_set_parent), deferred :: set_parent
      procedure(I_has_child), deferred :: has_child
      procedure(I_get_child), deferred :: get_child
      procedure(I_set_child), deferred :: set_child
      procedure(I_deallocate_child), deferred :: deallocate_child
      procedure(I_get_value), deferred :: get_value
      procedure(I_set_value), deferred :: set_value
      
      procedure(I_which_side_am_i), deferred :: which_side_am_i
      procedure(I_which_child), deferred :: which_child
      procedure(I_get_height), deferred :: get_height
      procedure(I_update_height), deferred :: update_height
   end type __base_node

   type, extends(__base_node) ::  __set_node
      type(__set_node), pointer :: parent => null()
      class(__base_node), allocatable :: left
      class(__base_node), allocatable :: right
      integer :: height=1
      __T_declare_component__ :: value
   contains
      procedure :: to_node => __MANGLE(to_node)
      procedure :: get_parent =>  __MANGLE(get_parent)
      procedure :: set_parent =>  __MANGLE(set_parent)
      procedure :: has_child => __MANGLE(has_child)
      procedure :: get_child => __MANGLE(get_child)
      procedure :: set_child => __MANGLE(set_child)
      procedure :: deallocate_child => __MANGLE(deallocate_child)
      procedure :: get_value => __MANGLE(get_value)
      procedure :: set_value => __MANGLE(set_value)
      
      procedure :: which_child => __MANGLE(which_child)
      procedure :: which_side_am_i => __MANGLE(which_side_am_i)

      procedure :: get_height => __MANGLE(get_height)
      procedure :: update_height => __MANGLE(update_height)
   end type __set_node

   abstract interface

      function I_to_node(this) result(node)
         import __base_node
         import __set_node
         type(__set_node), pointer :: node
         class(__base_node), target, intent(in) :: this
      end function I_to_node
      
      function I_get_parent(this) result(parent)
         import __base_node
         import __set_node
         type(__set_node), pointer :: parent
         class(__base_node), intent(in) :: this
      end function I_get_parent

      subroutine I_set_parent(this, parent)
         import __base_node
         import __set_node
         class(__base_node), intent(inout) :: this
         type(__set_node), pointer, intent(in) :: parent
      end subroutine I_set_parent

      logical function I_has_child(this, side) result(has_child)
         import __base_node
         class(__base_node), intent(in) :: this
         integer, intent(in) :: side
      end function I_has_child

      function I_get_child(this, side) result(child)
         import __base_node
         import __set_node
         type(__set_node), pointer :: child
         class(__base_node), target, intent(in) :: this
         integer, intent(in) :: side
      end function I_get_child

      subroutine I_set_child(this, side, node)
         import __base_node
         import __set_node
         class(__base_node), intent(inout) :: this
         integer, intent(in) :: side
         type(__set_node), allocatable, intent(inout) :: node
      end subroutine I_set_child

      subroutine I_deallocate_child(this, side)
         import __base_node
         class(__base_node), intent(inout) :: this
         integer, intent(in) :: side
      end subroutine I_deallocate_child

      function I_get_value(this) result(value)
         import __base_node
         import __set_node
         __T_declare_result__, pointer :: value
         class(__base_node), target, intent(in) :: this
      end function I_get_value

      subroutine I_set_value(this, value)
         import __base_node
         class(__base_node), intent(inout) :: this
         __T_declare_dummy__, intent(in) :: value
      end subroutine I_set_value

      
      integer function I_which_side_am_i(this) result(side)
         import __base_node
         class(__base_node), target, intent(in) :: this
      end function I_which_side_am_i

      integer function I_which_child(this, child) result(side)
         import __base_node
         import __set_node
         class(__base_node), intent(in) :: this
         type(__set_node), target, intent(in) :: child
      end function I_which_child

      integer function I_get_height(this) result(height)
         import __base_node
         class(__base_node), intent(in) :: this
      end function I_get_height

      subroutine I_update_height(this)
         import __base_node
         class(__base_node), intent(inout) :: this
      end subroutine I_update_height

   end interface


      

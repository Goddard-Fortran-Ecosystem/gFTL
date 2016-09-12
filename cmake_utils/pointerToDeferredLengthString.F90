module Foo_mod
   implicit none

   type Foo
      character(len=:), allocatable :: buffer
   contains
      procedure :: get
   end type Foo

contains

   function get(f) result(p)
      class (Foo), target, intent(in) :: f
      character(len=:), pointer :: p
      p => f%buffer
   end function get

end module Foo_mod

program main
   use Foo_mod
   implicit none
   
   character(len=:), pointer :: p
   type (Foo) :: f

   f%buffer = 'cat'
   p => f%get()

   if (.not. (p == 'cat')) then
      stop 1
   end if

end program main

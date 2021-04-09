module Foo_mod
   implicit none

   type Foo
      character(len=17) :: t
   contains
      procedure :: get
   end type Foo

contains

   function get(f) result(p)
      class (Foo), target, intent(in) :: f
      character(len=17), pointer :: p
      p => f%t
   end function get

end module Foo_mod

program main
   use Foo_mod
   implicit none
   
   character(len=17), pointer :: p
   type (Foo) :: f

   f%t = 'cat'
   p => f%get()
   print*,p

end program main

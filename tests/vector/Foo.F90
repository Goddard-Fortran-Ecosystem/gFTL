smodule Foo_mod
   use funit
   implicit none
   private

   public :: Foo
   public :: ChildOfFoo
   public :: operator(==)

  interface equal_to
     module procedure :: equal_to_foo
  end interface equal_to
   
   type :: Foo
      integer :: i = -1
   end type Foo

   type, extends(Foo) :: ChildOfFoo
      real :: x = 0.
   end type ChildOfFoo


contains

   logical function equal_base(a, b)
      type(Foo), intent(in) :: a
      type(Foo), intent(in) :: b

      equal_base = a%i == b%i
   end function equal_base

   
   logical function equal_child(a, b)
      type(ChildOfFoo), intent(in) :: a
      type(ChildOfFoo), Intent(in) :: b

      equal_child = (a%Foo == b%Foo) .and. a%x == b%x
   end function equal_child

end module Foo_mod

module Foo_mod
   use pFUnit_mod
   implicit none
   private

   public :: Foo
   public :: assertEqual

   type Foo
      integer :: i
   contains
      procedure :: equal
      generic :: operator(==) => equal
!!$      procedure :: copy
!!$      generic :: assignment(=) => copy
   end type Foo

   interface Foo
      module procedure newFoo
   end interface Foo

   interface assertEqual
      module procedure assertEqual_FooFoo
   end interface assertEqual

contains

   function newFoo(i) result(f)
      type (Foo) :: f
      integer, intent(in) :: i
      f%i = i
   end function newFoo
   

   logical function equal(a, b)
      class (Foo), intent(in) :: a
      class (Foo), intent(in) :: b
      equal = (a%i == b%i)
   end function equal


   subroutine assertEqual_FooFoo(a, b, message, location)
      class (Foo), intent(in) :: a
      class (Foo), intent(in) :: b
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      call assertEqual(a%i, b%i, message, location)
      
   end subroutine assertEqual_FooFoo


end module Foo_mod

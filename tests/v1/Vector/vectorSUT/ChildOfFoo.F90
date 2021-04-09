module ChildOfFoo_mod
   use pFUnit_mod
   use Foo_mod
   implicit none
   private

   public :: ChildOfFoo
   public :: assertEqual

   type, extends(Foo) :: ChildOfFoo
      integer :: j
   contains
      procedure :: equal
      procedure :: copy
   end type ChildOfFoo

   interface ChildOfFoo
      module procedure newChildOfFoo
   end interface ChildOfFoo


   interface assertEqual
      module procedure assertEqual_FooFoo
   end interface assertEqual

contains

   function newChildOfFoo(i,j) result(child)
      type (ChildOfFoo) :: child
      integer, intent(in) :: i
      integer, intent(in) :: j
   end function NewChildOfFoo


   subroutine copy(a, b)
      class (ChildOfFoo), intent(out) :: a
      class (Foo), intent(in) :: b

      a%i = b%i
      select type (b)
      class is (ChildOfFoo)
         a%j = b%j
      end select

   end subroutine copy


   logical function equal(a, b)
      class (ChildOfFoo), intent(in) :: a
      class (Foo), intent(in) :: b

      select type (b)
      class is (ChildOfFoo)
         equal = (a%i == b%i .and. a%j == b%j)
      class default
         equal = .false.
      end select

   end function equal


   subroutine assertEqual_FooFoo(a, b, message, location)
      type (ChildOfFoo), intent(in) :: a
      class (Foo), intent(in) :: b
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      select type (b)
      class is (ChildOfFoo)
         call assertEqual([a%i,a%j], [b%i,b%j], message, location)
      class default
         call throw('Dynamic types do not match.')
      end select
      
      
   end subroutine assertEqual_FooFoo
   
end module ChildOfFoo_mod

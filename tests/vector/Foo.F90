module Foo_mod
   use funit
   implicit none
   private

   public :: Foo
   public :: ChildOfFoo

   type, extends(Matchable) :: Foo
      integer :: i = -1
   contains
      procedure :: equals => equals_foo
      procedure :: describe_to => describe_to_foo
   end type Foo

   type, extends(Foo) :: ChildOfFoo
      real :: x = 0.
   contains
      procedure :: equals => equals_child_of_foo
      procedure :: describe_to => describe_to_child_of_foo
   end type ChildOfFoo

   interface Foo
      module procedure new_Foo_default
      module procedure new_Foo
   end interface Foo

   interface ChildOfFoo
      module procedure new_ChildOfFoo_default
      module procedure new_ChildOfFoo
   end interface ChildOfFoo

   type(Foo) :: zero
   type(ChildOfFoo) :: one
   type(Foo) :: two
   type(ChildOfFoo) :: three

contains


   function new_Foo_default() result(f)
      type(Foo) :: f
      f = Foo(-1)
   end function new_Foo_Default
   
   function new_Foo(i) result(f)
      type(Foo) :: f
      integer, intent(in) :: i
      f%i = i
      call f%set_type_name('Foo')
   end function new_Foo

   function new_ChildOfFoo_default() result(f)
      type(ChildOfFoo) :: f
      f = ChildOfFoo(-1,0.)
   end function new_ChildOfFoo_default
   
   function new_ChildOfFoo(i, x) result(f)
      type(ChildOfFoo) :: f
      integer, intent(in) :: i
      real, intent(in) :: x
      f%i = i
      f%x = x
      call f%set_type_name('ChildOfFoo')
   end function new_ChildOfFoo

   
   logical function equals_foo(this, other)
      class(Foo), intent(in) :: this
      class(*), intent(in) :: other

      select type (other)
      type is (Foo)
         equals_foo = this%i == other%i
      class default
         equals_foo = .false.
      end select

   end function equals_foo

   subroutine describe_to_foo(this, description)
      class(Foo), intent(in) :: this
      class(MatcherDescription), intent(inout) :: description

      character(100) :: buffer
      write(buffer,'(a,i0,a)') 'Foo(', this%i, ')'

      call description%append_text(trim(buffer))
      
   end subroutine describe_to_foo
         
   logical function equals_child_of_foo(this, other)
      class(ChildOfFoo), intent(in) :: this
      class(*), intent(in) :: other

      select type (other)
      type is (ChildOfFoo)
         equals_child_of_foo = (this%Foo == other%Foo) .and. this%x == other%x
      class default
         equals_child_of_foo = .false.
      end select
   end function equals_child_of_foo

   subroutine describe_to_child_of_foo(this, description)
      class(ChildOfFoo), intent(in) :: this
      class(MatcherDescription), intent(inout) :: description

      character(100) :: buffer
      write(buffer,'(a,i0,a1,g0,a)') 'ChildOfFoo(', this%i, ",", this%x, ')'
      call description%append_text(trim(buffer))
      
   end subroutine describe_to_child_of_foo

end module Foo_mod

module AbstractBar_mod
   use funit
   implicit none
   private

   public :: AbstractBar
   public :: ConcreteBarA
   public :: ConcreteBarB
   public :: operator(<)

#ifndef __GFORTRAN__
   type, abstract, extends(Matchable) :: AbstractBar
      integer :: i = -1
   contains
   end type AbstractBar

   type, extends(AbstractBar) :: ConcreteBarA
      real :: x = 0.
   contains
      procedure :: equals => equals_concrete_bar_A
      procedure :: describe_to => describe_to_concrete_bar_A
   end type ConcreteBarA

   type, extends(AbstractBar) :: ConcreteBarB
      logical :: flag = .true.
   contains
      procedure :: equals => equals_concrete_bar_B
      procedure :: describe_to => describe_to_concrete_bar_B
   end type ConcreteBarB

#else
   type, abstract :: AbstractBar
      integer :: i = -1
   contains
      procedure :: equals => equals_bar
      generic :: operator(==) => equals
!!$      procedure :: describe_to => describe_to_foo
   end type AbstractBar

   type, extends(AbstractBar) :: ConcreteBarA
      real :: x = 0.
   contains
      procedure :: equals => equals_concrete_bar_A
      procedure :: describe_to => describe_to_concrete_bar_A
   end type ConcreteBarA
   type, extends(AbstractBar) :: ConcreteBarB
      logical :: flag = .true.
   contains
      procedure :: equals => equals_concrete_bar_B
      procedure :: describe_to => describe_to_concrete_bar_B
   end type ConcreteBarA
#endif

   interface ConcreteBarA
      module procedure new_ConcreteBarA_default
      module procedure new_ConcreteBarA
   end interface ConcreteBarA

   interface ConcreteBarB
      module procedure new_ConcreteBarB_default
      module procedure new_ConcreteBarB
   end interface ConcreteBarB

   type(ConcreteBarA) :: zero
   type(ConcreteBarA) :: one
   type(ConcreteBarA) :: two
   type(ConcreteBarB) :: three

   interface operator(<)
      module procedure less
   end interface operator(<)

contains


   function new_ConcreteBarA_default() result(f)
      type(ConcreteBarA) :: f
      f%i = 0
      f%x = 0.
   end function new_ConcreteBarA_default
   
   function new_ConcreteBarA(i,x) result(f)
      type(ConcreteBarA) :: f
      integer, intent(in) :: i
      real, intent(in) :: x
      f%i = i
      f%x = x
#ifndef __GFORTRAN__
      call f%set_type_name('ConcreteBarA')
#endif
   end function new_ConcreteBarA

   function new_ConcreteBarB_default() result(f)
      type(ConcreteBarB) :: f
      f%i = 0
      f%flag = .false.
   end function new_ConcreteBarB_default
   
   function new_ConcreteBarB(i,flag) result(f)
      type(ConcreteBarB) :: f
      integer, intent(in) :: i
      logical, intent(in) :: flag
      f%i = i
      f%flag = flag
#ifndef __GFORTRAN__
      call f%set_type_name('ConcreteBarB')
#endif
   end function new_ConcreteBarB

   
   logical function equals_concrete_bar_A(this, other)
      class(ConcreteBarA), intent(in) :: this
      class(*), intent(in) :: other

      select type (other)
      type is (ConcreteBarA)
         equals_concrete_bar_A = (this%i == other%i) .and. (this%x == other%x)
      class default
         equals_concrete_bar_A = .false.
      end select

   end function equals_concrete_bar_A

   logical function equals_concrete_bar_B(this, other)
      class(ConcreteBarB), intent(in) :: this
      class(*), intent(in) :: other

      select type (other)
      type is (ConcreteBarB)
         equals_concrete_bar_B = (this%i == other%i) .and. (this%flag .eqv. other%flag)
      class default
         equals_concrete_bar_B = .false.
      end select

   end function equals_concrete_bar_B

   subroutine describe_to_concrete_bar_A(this, description)
      class(ConcreteBarA), intent(in) :: this
      class(MatcherDescription), intent(inout) :: description

      character(100) :: buffer
      write(buffer,'(a,i0,a)') 'ConcreteBarA(', this%i, "',", this%x, ')'

      call description%append_text(trim(buffer))
      
   end subroutine describe_to_concrete_bar_A
         
   subroutine describe_to_concrete_bar_B(this, description)
      class(ConcreteBarB), intent(in) :: this
      class(MatcherDescription), intent(inout) :: description

      character(100) :: buffer
      write(buffer,'(a,i0,a)') 'ConcreteBarB(', this%i, ",", this%flag, ')'

      call description%append_text(trim(buffer))
      
   end subroutine describe_to_concrete_bar_B
         
   logical function less(a, b)
      class(AbstractBar), intent(in) :: a
      class(AbstractBar), intent(in) :: b

      select type (a)
      type is (ConcreteBarA)
         select type (b)
         type is (ConcreteBarA)
            less = (a%i < b%i) .and. (a%x < b%x)
         class default
            less = .true.
         end select
      type is (ConcreteBarB)
         select type (b)
         type is (ConcreteBarB)
            less = (a%i < b%i) .and. (a%flag .and. .not. b%flag)
         class default
            less = .false.
         end select
      end select
   end function less

end module AbstractBar_mod

! This is an example for derived types defined by users

!----------------------------------------
! Module creating a base type
!----------------------------------------
module MyBase_mod

  public :: MyBase

  type,abstract:: MyBase
    real :: r
  contains
    procedure(display_),deferred :: display
  end type MyBase
  
  abstract interface
    subroutine display_(m1)
      import MyBase
      class(MyBase),intent(in) :: m1
    end subroutine display_
  end interface

end module MyBase_mod

!----------------------------------------
! Module creating a derived type
!----------------------------------------
module MyType_mod
  use MyBase_mod
  type,extends(MyBase) :: MyType
  contains
    procedure :: display
  end type

  interface MyType
    module procedure newMyType
  end interface

contains

  function newMyType(r) result(m)
    real,intent(in) :: r
    type(MyType) :: m
    m%r = r
  end function

  subroutine display(m1)
    class(MyType),intent(in) :: m1
    print *, m1%r
  end subroutine display

end module MyType_mod

!----------------------------------------
! Module creating a vector of MyBase objects
!----------------------------------------
module VecMyPolyPtr_mod
! Include any modules needed for the types used in the vector
  use MyBase_mod
! Specify the type of the elements of the vector
#define T MyBase
! Specify that the elements of the vector are polymorphic
#define T_polymorphic
! Include the vector template file to define the vector type
! The type will be called Vector
#include "vector/template.inc"
#undef T
#undef T_polymorphic
end Module

program main

  use MyType_mod
  use VecMyPolyPtr_mod
  implicit none
  type(MyType) ,target :: mt
  class(MyBase) ,pointer :: mp
  class(MyBase) ,pointer :: mp_vector_accessor
  type(Vector) :: mv

  mt= myType(1.0d0)
  mp=>mt

  ! Add an element to the vector
  call mv%push_back(mp)

  ! Show the size of the vector
  print *, "mv%size()    =    ", mv%size()

  ! Access an element of the vector
  mp_vector_accessor => mv%at(1)

  ! Use the element as you would a derived type object normally
  call mp_vector_accessor%display()

end program main

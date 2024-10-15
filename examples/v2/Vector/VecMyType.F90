! This is an example for derived types defined by users

!----------------------------------------
! Module creating a derived type
!----------------------------------------
module MyType_mod
  public :: MyType
  type MyType
    real :: r
  contains
    procedure :: equal
    procedure :: display
    generic :: operator(==) => equal
  end type myType

  interface MyType
    module procedure newMyType
  end interface

contains
  function newMyType(r) result(m)
    real,intent(in) :: r
    type(MyType) :: m
    m%r = r
  end function

  logical function equal(m1,m2)  result(l)
    class(MyType),intent(in) :: m1,m2
    l = (abs(m1%r- m2%r) <= 1.0e-7)
  end function equal

  subroutine display(m1)
    class(MyType),intent(in) :: m1
    print *, m1%r
  end subroutine display

end module MyType_mod

!----------------------------------------
! Module creating a vector of MyType objects
!----------------------------------------
module VecMyType_mod
! Include any modules needed for the types used in the vector
  use MyType_mod
! Specify the type of the elements of the vector
#define T MyType
! Define the == operator for elements of the vector.
! This allows two vectors to be compared with the equal method
#define T_EQ(x,y) (x == y)
! Define the < operator for elements of the vector.
! This allows two vectors to be compared with the relational methods
#define T_LT(x,y) (x%r < y%r)
! Include the vector template file to define the vector type
! The type will be called Vector
#include "vector/template.inc"

end module VecMyType_mod

program main

  use MyType_mod
  use VecMyType_mod

  implicit none
  type (Vector) :: mv
  type (MyType), pointer :: mv_p

  ! Initialise the vector type
  mv=Vector()

  ! Add an element to the vector
  call mv%push_back(MyType(2.0))

  ! Access an element of the vector
  mv_p => mv%at(1)

  ! Use the element as you would a derived type object normally
  call mv_p%display()

end program main


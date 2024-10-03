! This is an example for derived types defined by users

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

! Define the vector for the derived type
! It is shown in types/Foo.inc

module VecMyType_mod
  use MyType_mod
#define T MyType
#define T_EQ(x,y) (x == y)
#define T_LT(x,y) (x%r < y%r)
#include "vector/template.inc"

end module VecMyType_mod

program main

  use MyType_mod
  use VecMyType_mod

  implicit none
  type (Vector) :: mv
  type (MyType), pointer :: mv_p
  mv=Vector()
  call mv%push_back(MyType(2.0))

  mv_p => mv%at(1)
  call mv_p%display()

end program main


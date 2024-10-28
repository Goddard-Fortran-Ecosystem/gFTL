! This is an example for derived types defined by users
 
module MyType_mod
   public :: MyType
   type MyType
      real :: r
   contains
      procedure :: equal
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

end module MyType_mod

! Define the vector for the derived type
! It is shown in types/Foo.inc

module VecMyType_mod
   use MyType_mod
#define _type type(MyType)
#define _equal_defined
#define _LESS_THAN(x,y) (x%r < y%r)
#include "templates/vector.inc"

end module VecMyType_mod

program main

   use MyType_mod
   use VecMyType_mod

   implicit none
   type (Vector) :: mv
   mv=Vector()
   call mv%push_back(MyType(2.0))

end program main

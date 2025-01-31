! This is an example for derived types defined by users
 
module MyBase_mod

   public :: MyBase

   type,abstract:: MyBase
      real :: r
   contains
      procedure(equal_),deferred :: equal
      generic :: operator(==) => equal
   end type MyBase

   abstract interface
      logical function equal_(m1,m2)  result(l)
         import MyBase
         class(MyBase),intent(in) :: m1
         class(MyBase),intent(in) :: m2
      end function equal_
   end interface

end module MyBase_mod

module MyType_mod
   use MyBase_mod
   type,extends(MyBase) :: MyType
   contains
      procedure :: equal
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

   logical function equal(m1,m2)  result(l)
      class(MyType),intent(in) :: m1
      class(MyBase),intent(in) :: m2
      l = (abs(m1%r- m2%r) <= 1.0e-7)
   end function equal

end module MyType_mod

module VecMyPolyPtr_mod
   use MyBase_mod
#define _type class(MyBase)
#define _allocatable
#define _pointer
#define _equal_defined
#include "templates/vector.inc"
end Module

program main

   use MyType_mod
   use VecMyPolyPtr_mod 
   implicit none
   type(MyType) ,target :: mt
   class(MyBase) ,pointer :: mp
   type(Vector) :: mv

   mt= myType(1.0d0)
   mp=>mt

   call mv%push_back(mp)
   
end program main

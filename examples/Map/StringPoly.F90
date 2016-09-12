module Base_mod
   public :: Base
   type,abstract :: Base
      character(len=10) :: s
   contains
      procedure :: printb
   end type
   contains
      subroutine printb(this)
         class(Base),intent(in) :: this
         print*,this%s
      end subroutine
end module Base_mod

module Derive1_mod
   use Base_mod
   public :: Derive1
   type,extends(Base) :: Derive1
   end type
   interface Derive1
      module procedure newDerive1
   end interface

   contains
      function newDerive1(s1) result(d1)
       character(len=*),intent(in) ::s1
       type(Derive1) :: d1
       d1%s = trim(s1)
      end function
end module Derive1_mod

module Derive2_mod
   use Base_mod
   public :: Derive2
   type,extends(Base) :: Derive2
   end type
   interface Derive2
      module procedure newDerive2
   end interface

   contains
      function newDerive2(s2) result(d2)
       character(len=*),intent(in) :: s2
       type(Derive2) :: d2
       d2%s = trim(s2)
      end function
end module Derive2_mod

module  StringPoly_mod

   use Base_mod

#define _key_string_deferred
#define _key_equal_defined
#define _Key_less_than_defined

#define _value class(Base)
#define _value_allocatable

#include "templates/map.inc"
end module StringPoly_mod


program main
   use Derive1_mod
   use Derive2_mod
   use StringPoly_mod
   implicit none
   type (Map) :: m
   type (MapIterator) :: iter
   class(Base) ,pointer :: p
   character(len=:),pointer :: cp

   call m%insert('d1',Derive1('1Derive1'))
   call m%insert('d2',Derive2('1Derive2'))

   p=>m%at('d1')
   call p%printb()
   p=>m%at('d2')
   call p%printb()
   
   iter=m%begin()
   !
   ! error to acess key()
   !
    cp=>iter%key()
   print*,cp

   p=>iter%value()
   call p%printb()

end program main

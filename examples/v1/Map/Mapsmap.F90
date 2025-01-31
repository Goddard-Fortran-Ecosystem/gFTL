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
#define _key_less_than_defined

#define _value class(Base)
#define _value_allocatable
#define _alt
#include "templates/map.inc"
end module StringPoly_mod

module tracer_mod
   use Derive1_mod
   use Derive2_mod
   use StringPoly_mod,only : StringPolyMap=>map,StringMapIterator=>MapIterator
   implicit none

   type :: Tracer
     type(StringPolyMap) :: aSmap
   contains
   end type
end module

module  TracerMap_mod
   use Tracer_mod
#define _key_string_deferred
#define _key_equal_defined
#define _Key_less_than_defined

#define _value class(Tracer)
#define _value_allocatable

#define _alt
#include "templates/map.inc"
end module TracerMap_mod

module TracerBundle_mod
   use Tracer_mod
   use TracerMap_mod,TracerMap=>Map
   implicit none
   type :: TracerBundle
     type(TracerMap) :: aTmap
   contains
   end type
end module TracerBundle_mod
program main
   use Derive1_mod
   use Derive2_mod
   use StringPoly_mod
   use Tracer_mod

   use TracerMap_mod,TracerMapIterator=>MapIterator

   use TracerBundle_mod

   implicit none
   type (StringPolyMap) :: m
   type (StringMapIterator) :: mp
   class(Base) ,pointer :: p
   type(Tracer) :: aTrcer
   type(Tracer),pointer :: aTp
   type(TracerBundle) :: aBundle
   type(TracerMapIterator) :: iter

   call m%insert('d1',Derive1('1Derive1'))
   call m%insert('d2',Derive2('1Derive2'))

   p=>m%at('d1')
   call p%printb()
   p=>m%at('d2')
   call p%printb()

   mp = m%find('d1')

   print*,mp%key()

   call aTrcer%aSmap%insert('d1',Derive1('1Derive1'))

   call aBundle%aTmap%insert('firstT',aTrcer)
   iter = aBundle%aTmap%begin()
   print*,iter%key()

   aTp=>iter%value()
   p=>aTp%aSmap%at('d1')

   call p%printb()

end program main

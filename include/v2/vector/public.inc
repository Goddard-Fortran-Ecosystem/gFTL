   private
   public :: __vector
   public :: __vector_iterator
   public :: __vector_riterator

   ! Non type-bound procedures
   public :: swap

   ! Iterator functions
   public :: advance
   public :: begin
   public :: distance
   public :: end
   public :: next
   public :: prev
   public :: operator(+)
   public :: operator(-)


   ! Iterator and possibly container functions:
   ! The following are always defined for the iterator.
   ! May not also be defined for the container itself depending
   ! on properties of parameter T.

   public :: operator(==)
   public :: operator(/=)

   public :: operator(<)
   public :: operator(>)
   public :: operator(<=)
   public :: operator(>=)


#include "algorithms/public.inc"
   

   private
   
   public :: __set
   public :: __set_iterator
   
   ! Non type-bound procedures
   public :: swap

   ! Iterator functions
   public :: advance
   public :: begin
   public :: end
   public :: next
   public :: prev

   public :: operator(==)
   public :: operator(/=)


#include "algorithms/public.inc"

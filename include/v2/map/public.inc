   private ! except for
   public :: __map
   public :: __map_iterator
   public :: __map_pair
   
   ! Non type-bound procedures
   public :: swap

   ! Iterator functions
   public :: advance
   public :: begin
   public :: end
   public :: ftn_begin
   public :: ftn_end
   public :: next
   public :: prev

   public :: operator(==)
   public :: operator(/=)

#include "algorithms/public.inc"

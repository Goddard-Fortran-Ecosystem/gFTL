   private
   
   public :: __alt_set
   public :: __alt_set_iterator
   
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

   private ! except for
   
   public :: __omap
   public :: __omap_iterator
   public :: __omap_pair
   
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

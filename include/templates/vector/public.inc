   private
   public :: __vector

   ! Non type-bound procedures
   public :: swap

#include "parameters/T/copy_vector_T_to_internal_T.inc"
#include "parameters/T/define_derived_macros.inc"

#if defined(__T_EQ__) || defined(__T_LT__)
   public :: operator(==)
   public :: operator(/=)
#endif
#ifdef __T_LT__
   public :: operator(<)
   public :: operator(>)
   public :: operator(<=)
   public :: operator(>=)
#endif
#include "parameters/T/undef_derived_macros.inc"
#include "parameters/T/undef_internal.inc"

   ! iterators
   public :: __vector_iterator
   public :: __vector_riterator
   public :: advance
   public :: begin
   public :: distance
   public :: end
   public :: next
   public :: prev
   public :: operator(+)
   public :: operator(-)

   

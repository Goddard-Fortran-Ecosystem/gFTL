#include "parameters/T1/define_derived_macros.inc"
#include "parameters/T2/define_derived_macros.inc"

   public :: __pair
   public :: swap

   type :: __pair
      private
      __T1_declare_component__ :: first
      __T2_declare_component__ :: second
   contains
!!$      procedure :: write_formatted => __GUARD(get)
      procedure :: get_first => __GUARD(get_first)
      procedure :: get_second => __GUARD(get_second<)
   end type __pair


   interface swap
      module procedure __GUARD(swap)
   end interface

#include "parameters/T1/undef_derived_macros.inc"
#include "parameters/T2/undef_derived_macros.inc"


   

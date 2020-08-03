#include "parameters/T1/define_derived_macros.inc"
#include "parameters/T2/define_derived_macros.inc"
#define __guard __pair_guard

   type :: __pair
      private
      __T1_declare_component__ :: first
      __T2_declare_component__ :: second
   contains
!!$      procedure :: write_formatted => __MANGLE(get)
      procedure :: get_first => __MANGLE(get_first)
      procedure :: get_second => __MANGLE(get_second<)
   end type __pair


   interface swap
      module procedure __MANGLE(swap)
   end interface swap

#undef __guard
#include "parameters/T1/undef_derived_macros.inc"
#include "parameters/T2/undef_derived_macros.inc"


   

!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

#ifndef __tmplbase_inc
#  define __tmplbase_inc

#  define __QUOTE(x) x
#  define __PROC(x) __QUOTE(__container_prefix)__QUOTE(x)

#  define __ASSIGN_DIM(dest, src) dest=src


#ifndef __GFORTRAN
#  define __IMPURE_ELEMENTAL
#else
#  define __IMPURE_ELEMENTAL impure elemental
#endif

! 64 bit integers are necessary to support containers with > 2**32 items.
! While F2008 makes INT64 standard, vendors are permitted to give it a negative
! value and not support integers of that kind.   We switch to 32 bit
! integers in that case.
      implicit none

      integer, parameter :: SIZE_KIND =                                        &
     &           max(kind(1),selected_int_kind(18))

      ! Private type used to force keyword access for 
      ! optional arguments.
      type Unusable
      end type Unusable

! Assume 64 bit is supported by default
#ifndef SUPPORT_FOR_INT64
#  ifndef NO_SUPPORT_FOR_INT64
#    define SUPPORT_FOR_INT64
#  endif
#endif

#endif /* #ifndef __tmplbase_inc */


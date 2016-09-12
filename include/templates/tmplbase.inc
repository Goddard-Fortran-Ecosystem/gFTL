#ifndef __tmplbase_inc
#  define __tmplbase_inc

#  define __QUOTE(x) x
#  define __PROC(x) __QUOTE(__container_prefix)__QUOTE(x)

! The Intel compiler requires a flag to use F2003 allocate-on-assignment
! semantics.  To avoid assuming that users have that flag set,
! we use the more verbose option here.  Unfortunately, gfortran does not
! support this variant for arrays, so we do use the allocate-on-assignment
! for that compiler.
#  ifdef __INTEL_COMPILER
#    define __ASSIGN_DIM(dest, src) allocate(dest,source=src)
#  else
#    define __ASSIGN_DIM(dest, src) dest=src
#  endif


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


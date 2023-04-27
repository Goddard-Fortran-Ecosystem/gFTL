#ifndef __gftl_common

#    define __gftl_common

! Per-container name mangling   
#    define __IDENTITY(x) x
#    define __COMMA(x) , x
#ifdef _CRAYFTN
#    define __MANGLE(proc) __guard##proc
#else
#    define __MANGLE(proc) __IDENTITY(__guard)__IDENTITY(proc)
#endif
!#    define __MANGLE(proc) __IDENTITY(proc)__IDENTITY(_)__IDENTITY(__guard)

! Fortran portability support
#    ifdef  gftl_size_kind
#        define __gftl_size_kind gftl_size_kind
#    else
!        Note: F2018 guarantees that there is at least one integer
!        kind that supports 18 digits.  For all extant compilers (July
!        2020), this will be different than default integer
#        define __gftl_size_kind selected_int_kind(18)
#    endif
#    ifdef gftl_disable_index_kind_overload
#        define __gftl_disable_index_kind_overload
#    endif


! Predefined intrinsic types

! Note: Fortran preprocessors do not consistently intrepret octal
! constants, so we have to do some careful arithmetic in base 10
! instead of the more elegant approach possible in C.

   
#define __LOGICAL__               1   
#define __INTEGER__               2
#define __REAL__                  3
#define __COMPLEX__               4
#define __CHARACTER__             5
#define __DOUBLE_PRECISION__      6
#define __UNLIMITED_POLYMORPHIC__ 7
#define __DOUBLE_COMPLEX__        8

#define __LOGICAL   8

#define __INTEGER   16
#define __INTEGER8  __INTEGER+3
#define __INTEGER16 __INTEGER+4
#define __INTEGER32 __INTEGER+5
#define __INTEGER64 __INTEGER+6

#define __REAL      24
#define __REAL16    __REAL+4
#define __REAL32    __REAL+5
#define __REAL64    __REAL+6
#define __REAL128   __REAL+7

#define __COMPLEX      32
#define __COMPLEX16    __COMPLEX+4
#define __COMPLEX32    __COMPLEX+5
#define __COMPLEX64    __COMPLEX+6
#define __COMPLEX128   __COMPLEX+7

#define __CHARACTER             40
#define __CHARACTER_DEFERRED    41
#define __DOUBLE_PRECISION      48
#define __UNLIMITED_POLYMORPHIC 56
#define __DOUBLE_COMPLEX        64


! Attributes
! Note 1. Polymorphic implies allocatable (now that POINTER is no longer supported).   
! Note 2. Deferred also implies allocatable
! Note 3
#define __POLYMORPHIC 1
#define __DEFERRED 2


! Type parameters (confusing terminology)
! _rank  - default scalar (0), implies deferred
! _ubound - default absent
! _lbound - default absent unless _ubound is defined, in which case default is 1+0*_ubound
   
   implicit none
#    include "shared/KeywordEnforcer.inc"
#    include "shared/error_codes.inc"


#    define __UNUSED_DUMMY(x) if (.false.) print*,shape(x)

   integer, parameter :: GFTL_SIZE_KIND = __gftl_size_kind
   type :: NO_TYPE_
   end type NO_TYPE_
   type(NO_TYPE_), parameter :: NO_TYPE__ = NO_TYPE_()

#endif


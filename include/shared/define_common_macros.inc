#ifndef __gftl_common

#    define __gftl_common

! Per-container name mangling   
#    define __IDENTITY(x) x
#    define __GUARD(proc) __IDENTITY(__guard)__IDENTITY(proc)


! Fortran portability support
#    ifdef _gftl_impure_elemental
#        define __gftl_impure_elemental _gftl_impure_elemental
#    endif

#    ifdef _gftl_size_kind
#        define __gftl_size_kind _gftl_size_kind
#    else
!        Note: F2018 guarantees that there is at least one integer kind that supports 18 digits
!        But, just in case, we fall back to default integer.
#        define __gftl_size_kind max(kind(1),selected_int_kind(18))
#    endif
#    ifdef _gftl_disable_index_kind_overload
#        define __gftl_disable_index_kind_overload
#    endif


! Predefined intrinsic types

#define __LOGICAL__          1   
#define __INTEGER__          2
#define __REAL__             3
#define __COMPLEX__          4
#define __CHARACTER__        5
#define __DOUBLE_PRECISION__ 6

#define __LOGICAL   010

#define __INTEGER   020
#define __INTEGER8  023
#define __INTEGER16 024
#define __INTEGER32 025
#define __INTEGER64 026

#define __REAL      030
#define __REAL16    034
#define __REAL32    035
#define __REAL64    036
#define __REAL128   037

#define __COMPLEX      040
#define __COMPLEX16    044
#define __COMPLEX32    045
#define __COMPLEX64    046
#define __COMPLEX128   047

#define __CHARACTER          050
#define __CHARACTER_DEFERRED 051
#define __DOUBLE_PRECISION   060


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


#endif


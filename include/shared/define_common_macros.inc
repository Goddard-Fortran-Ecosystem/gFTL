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

#define __INTEGER 1
#define __REAL 2
#define __DOUBLE_PRECISION 3
#define __COMPLEX 4
#define __LOGICAL 5
#define __CHARACTER 6


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


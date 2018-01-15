!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

changecom()
define(_param,_`'param())
define(__param,__`'param())
define(_PARAM,_`'translit(param(),`a-z',`A-Z'))
define(__PARAM,__`'translit(param(),`a-z',`A-Z'))
ifelse(param,type,`define(`_base',)',`define(`_base',_param())')
ifelse(param,type,`define(`_BASE',)',`define(`_BASE',_PARAM())')

! These macros are used in tests, but are not needed for the container
! templates.
#ifdef _base()_pointer

#  if defined(_base()_allocatable)

#    define __param()_allocatable_target
#    define __PARAM()_INIT_TARGET(trg, val) allocate(trg, source=val)

#  elif defined (_base()_rank) & (_base()_rank > 0) & !defined(_base()_extents)
   ! The Intel compiler requires a flag to use F2003 allocate-on-assignment
   ! semantics.  To avoid assuming that users have that flag set,
   ! we use the more verbose option here.  Unfortunately, gfortran does not
   ! support this variant for arrays, so we do use the allocate-on-assignment
   ! for that compiler.
#    define __param()_allocatable_target
#    ifdef __INTEL_COMPILER
#      define __PARAM()_INIT_TARGET(trg, val) allocate(trg, source=val)
#    else
#      define __PARAM()_INIT_TARGET(trg, val)  trg = val
#    endif

#  elif defined(_base()_len) & (_base()_len < 0)

#    define __param()_allocatable_target
#    define __PARAM()_INIT_TARGET(trg, val) allocate(trg, source=val)

#  else

#    define __PARAM()_INIT_TARGET(trg, val) trg = val

#  endif

#endif
      

#ifdef _base()_pointer
#  ifdef _base()_procedure
#    define __PARAM()_INIT(var, val, trg) var => val
#    define __param()_declare_local __param()_declare_type, pointer
#  else
#    define __PARAM()_INIT(var, val, trg) __PARAM()_INIT_TARGET(trg, val); var => trg
#  endif
#else
#  define __PARAM()_INIT(var, val, trg) __PARAM()_ASSIGN(var,val)
#endif

#ifndef __param()_declare_local
#  define __param()_declare_local __param()_declare_component
#endif


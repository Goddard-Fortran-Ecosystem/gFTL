changecom()
define(_param,__use_`'param())
define(__param,__`'param())
define(_PARAM,__USE_`'translit(param(),`a-z',`A-Z'))
define(__PARAM,__`'translit(param(),`a-z',`A-Z'))
ifelse(param,type,`define(`_base',__use)',`define(`_base',_param())')
ifelse(param,type,`define(`_BASE',__USE)',`define(`_BASE',`'_PARAM())')

! Althoug the code below could be compressed to some degree,
! maintainability is enhanced by isolating each complication in
! its own decision block.
!
! User settable tokens:
!    (a)  _param()
!    (b)  _base()_rank  and _base()_extents
!    (c)  _base()_string and _base()_string_deferred
!    (d)  _base()_logical
!    (e)  _base()_pointer
!    (f)  _base()_allocatable
!    (g)  _base()_procedure (not complete)
!
!    _BASE()_EQ
!
! Output macros
!
!    (a)  __PARAM()_ASSIGN(dest,src)  ! Set dest to have value src
!    (b)  __PARAM()_MOVE(dest,src)    ! If allocatable, move memory for src to variable dest
!                               ! otherwise, behave as __PARAM()_ASSIGN(dest,src)
!    (c)  __PARAM()_FREE(x)      ! Release memory associated with x (if allocatable)
!
! Output tokens
!
!    (a) __param()_declare_component
!    (b) __param()_declare_target
!    (c) __param()_declare_dummy
!    (d) __param()_declare_result
!    (e) __param()_interface ! unused - keeping for later use with procedure pointers
!
! Other tokens are for internal use in this file and should be undefined at the end.

!-------------------------------------------------------------------------------
! 1) Declared type

#if defined(_base()_string) | defined(_base()_string_deferred)
#  define __param()_target_type character(len=*)
#  if defined(_base()_string)
#    define __param()_declare_type character(len=_base()_string)
#  else
#    define __param()_declare_type character(len=:)
#  endif
#else
#  define __param()_declare_type _param()
#  if defined(_base()_procedure)
#    define __param()_interface
#    define __param()_target_type __param()_declare_type
#  else
#    define __param()_target_type __param()_declare_type
#  endif
#endif


!-------------------------------------------------------------------------------
! 2) Dimensions
!    (a) There are two cases to consider: deferred shape and non-deferred shape.
!    (b) Return pointers are always deferred shape.
#if defined (_base()_rank)
#  define __param()_rank _base()_rank
#else
#  define __param()_rank 0
#endif

#if __param()_rank == 0
#  define __param()_deferred_dim_attr
#elif (__param()_rank == 1)
#  define __param()_deferred_dim_attr , dimension(:)
#elif (__param()_rank == 2)
#  define __param()_deferred_dim_attr , dimension(:,:)
#elif (__param()_rank == 3)
#  define __param()_deferred_dim_attr , dimension(:,:,:)
#elif (__param()_rank == 4)
#  define __param()_deferred_dim_attr , dimension(:,:,:,:)
#elif (__param()_rank == 5)
#  define __param()_deferred_dim_attr , dimension(:,:,:,:,:)
#endif

#ifdef _base()_extents
#  define __param()_dimension_attr , dimension _base()_extents
#else
#  define __param()_dimension_attr __param()_deferred_dim_attr
#endif

#define __param()_result_dimension_attr __param()_deferred_dim_attr


!-------------------------------------------------------------------------------
! 3) Does the type need to be wrapped
#if defined(_base()_pointer)
#  define __param()_wrapped
#elif defined(_base()_allocatable)
#  define __param()_wrapped
#elif defined(_base()_string_deferred)
#  define __param()_wrapped
#elif __param()_rank > 0
#  define __param()_wrapped
#endif




!-------------------------------------------------------------------------------
! 4) Attributes for component declaration

#if defined(_base()_pointer)
#  if defined(_base()_procedure)
#    define __param()_component_attrs , pointer, nopass
#  else
#    define __param()_component_attrs __param()_dimension_attr, pointer
#  endif
#elif defined(_base()_allocatable) | defined(_base()_string_deferred)
#  define __param()_component_attrs __param()_dimension_attr, allocatable
#elif (__param()_rank > 0)
#  if defined(_base()_extents)
#     define __param()_component_attrs __param()_dimension_attr
#  else
#     define __param()_component_attrs __param()_dimension_attr, allocatable
#  endif
#else
#  define __param()_component_attrs
#endif

! macros for testing equality

#ifdef _BASE()_EQ
#  define __PARAM()_EQ _BASE()_EQ
#else
#  ifdef _base()_pointer
#    define __PARAM()_EQ(x,y) associated(x,y)
#  else
#    ifdef _BASE()_EQ_ELEMENT
#      define __PARAM()_EQ_ELEMENT(x,y) _BASE()_EQ_ELEMENT(x,y)    
#    else
#      ifdef _base()_logical 
#         define __PARAM()_EQ_ELEMENT(x,y) (x .eqv. y)
#      else
#        define __PARAM()_EQ_ELEMENT(x,y) (x == y)
#      endif
#    endif
!    Array support
#    if (_base()_rank > 0)
#       ifdef _base()_extents
!         Assumes that _BASE()_EQ_ELEMENT is an elemental function.  If not
!         then the user must define their own __PARAM()_EQ(x,y).
#         define __PARAM()_EQ(x,y) all(__PARAM()_EQ_ELEMENT(x,y))
#       else
#         define __PARAM()_EQ(x,y) __PROC(eqArray)(x,y)
#       endif
#    else
#      define __PARAM()_EQ(x,y) __PARAM()_EQ_ELEMENT(x,y)
#    endif
#  endif
#endif


! macros for comparing order
! User can specify (or override):
#define __param()_compare_well_defined
#ifdef _BASE()_LESS_THAN
#  define __PARAM()_LESS_THAN(x,y) _BASE()_LESS_THAN(x,y)
#else
#  if defined(_base()_string) | defined(_base()_string_deferred)
#    define __PARAM()_LESS_THAN(x,y) (x)<(y)
#  elif defined(_base()_less_than_defined)
#    define __PARAM()_LESS_THAN(x,y) (x)<(y)
#  else
#    undef __param()_compare_well_defined
! In most cases, we can provide a compare operator.  Not recommended for vector,
! but useful for set and keys for map:
#    if  !(defined(_base()_allocatable) & !defined(_base()_pointer))
#      define __PARAM()_LESS_THAN(x,y) defaultLessThan(x,y)
#      define __param()_needs_default_compare
#    endif
#  endif
#endif





!-------------------------------------------------------------------------------
! 5) Attributes for target and dummy declaration

#if defined(_base()_pointer)
#  if defined(_base()_procedure)
#    define __param()_target_attrs
#    define __param()_dummy_attrs __param()_target_attrs
#  else
#    define __param()_target_attrs __param()_dimension_attr, target
#    define __param()_dummy_attrs __param()_dimension_attr, target
#  endif
#else
#  define __param()_target_attrs  __param()_dimension_attr
#  define __param()_dummy_attrs __param()_dimension_attr
#endif


!-------------------------------------------------------------------------------
! 6) Attributes for function result declaration
!    Always used deferred shape here as pointer cannot work 
!    with non-deferred shape.

#define __param()_result_attrs __param()_deferred_dim_attr



!-------------------------------------------------------------------------------
! 8) Assembly

#define __param()_declare_component __param()_declare_type __param()_component_attrs
#define __param()_declare_target    __param()_target_type __param()_target_attrs
#define __param()_declare_dummy     __param()_target_type __param()_dummy_attrs
#define __param()_declare_result    __param()_declare_type __param()_result_attrs


!-------------------------------------------------------------------------------
! 9) Macros that manipulate storage

#ifdef _BASE()_ASSIGN
#  define __PARAM()_ASSIGN(dest,src) _BASE()_ASSIGN(dest,src)   
#  define __PARAM()_MOVE(dest,src) _BASE()_MOVE(dest,src)   
#  define __PARAM()_FREE(x) _BASE()_FREE(x)
#else
#  ifdef _base()_pointer

#    define __PARAM()_ASSIGN(dest,src)  dest=>src
#    define __PARAM()_MOVE(dest,src)  dest=>src
!#    define __PARAM()_FREE(x)  nullify(x)
#    define __PARAM()_FREE(x)

#  elif defined(_base()_allocatable)

#    define __PARAM()_ASSIGN(dest,src)  allocate(dest, source=src)
#    define __PARAM()_MOVE(dest,src)  call move_alloc(from=src, to=dest)
#    define __PARAM()_FREE(x)  deallocate(x)

#  elif defined (_base()_rank) & (_base()_rank > 0) & !defined(_base()_extents)
#    define __PARAM()_ASSIGN(dest,src) __ASSIGN_DIM(dest,src)
#    define __PARAM()_MOVE(dest,src) call move_alloc(from=src, to=dest)
#    define __PARAM()_FREE(x)  deallocate(x)

#elif defined (_base()_string_deferred)

#    define __PARAM()_ASSIGN(dest,src)  dest=src
#    ifdef __GFORTRAN__
#      define __PARAM()_MOVE(dest,src) dest=src;deallocate(src)
#    else
#    define __PARAM()_MOVE(dest,src) call move_alloc(from=src, to=dest)
#    endif
#    define __PARAM()_FREE(x)  deallocate(x)

#else

#    define __PARAM()_ASSIGN(dest,src)  dest=src
#    define __PARAM()_MOVE(dest,src) dest=src
#    define __PARAM()_FREE(x)

#  endif
#endif




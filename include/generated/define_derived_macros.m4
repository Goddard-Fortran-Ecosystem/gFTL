changecom()
define(__T,`__`'_T()')

   ! Input tokens:
   !   Mandatory
   !     __T
   !
   !   Optional
   ! 
   !     __T()_deferred
   !     __T()_polymorphic
   !     __T()_kindlen

#if __T() > 0
   ! Instrinsic types

   ! Some older compilers do not support declaring intrinsics with TYPE,
   ! e.g., TYPE(integer), so we suppress that here.

   ! Special per-type settings for different intrinsics:
#    if __T() == __COMPLEX

#        define __T_declare(kindlen) __IDENTITY(complex)__IDENTITY(kindlen)
#        define __T()_eq(a,b) a == b

#    elif __T() == __LOGICAL

#        define __T_declare(kindlen) __IDENTITY(logical)__IDENTITY(kindlen)
#        define __T()_eq(a,b) a .eqv. b

#    else

!        Most intrinsics use consistent operators
#        define __T()_eq(a,b) a == b
#        define __T()_ne(a,b) a /= b
#        define __T()_le(a,b) a <= b
#        define __T()_ge(a,b) a >= b
#        define __T()_lt(a,b) a <  b
#        define __T()_gt(a,b) a >  b

#        if __T() == __INTEGER
#            define __T_declare(kindlen) __IDENTITY(integer)__IDENTITY(kindlen)

#        elif __T() == __REAL
#            define __T_declare(kindlen) __IDENTITY(real)__IDENTITY(kindlen)

#        elif __T() == __CHARACTER
#            define __T_declare(kindlen) __IDENTITY(character)__IDENTITY(kindlen)
#            ifdef __T()_deferred
#                define __T()_kindlen__(context) (len=context)
#            endif
#        endif

#    endif
#else
! User defined derived type (or C_PTR, etc)
#    if  __T()_polymorphic
#        define __T()_declare(t) class(t)
#    else
#        define __T()_declare(t) type(t)
#    endif
#endif

#ifdef __T()_kindlen
#    define __T()_kindlen__(context) __T()_kindlen(context)
#else
#    ifndef __T()_kindlen__
#        define __T()_kindlen__(context)
#    endif
#endif

#ifdef __T()_kindlen_dummy
#    define __T()_kindlen_dummy__ __T()_kindlen_dummy
#else
#    define __T()_kindlen_dummy__ __T()_kindlen__(*)
#endif

#ifdef __T()_kindlen_component
#    define __T()_kindlen_component__ __T()_kindlen_component
#else
#    define __T()_kindlen_component__ __T()_kindlen__(:)
#endif

#ifdef __T_rank
#    if __T_rank == 1
#        define __T_dimension_result , dimension(:)
#    elif __T_rank == 2
#        define __T_dimension_result , dimension(:,:)
#    elif __T_rank == 3
#        define __T_dimension_result , dimension(:,:,:)
#    elif __T_rank == 4
#        define __T_dimension_result , dimension(:,:,:,:)
#    elif __T_rank == 5
#        define __T_dimension_result , dimension(:,:,:,:,:)
#    elif __T_rank == 6
#        define __T_dimension_result , dimension(:,:,:,:,:,:)
#    elif __T_rank == 7
#        define __T_dimension_result , dimension(:,:,:,:,:,:,:)
#    endif
#    if defined(__T_shape)
#        define __T_dimension_component , __IDENTITY(dimension)__IDENTITY(__T_shape)
#    else
#        define __T_dimension_component __T_dimension_result
#    endif
#    define __T_dimension_dummy __T_dimension_component
#else
#    define __T_dimension_result
#    define __T_dimension_component
#    define __T_dimension_dummy
#endif

#if defined(__T_deferred) || defined(__T_polymorphic) || (defined(__T_rank) && !defined(__T_shape))
#   define __T_allocatable , allocatable
#else
#   define __T_allocatable
#endif

#define __T()_declare_component __IDENTITY(__T()_declare(__T()_kindlen_component__))__IDENTITY(__T_dimension_component)__IDENTITY(__T_allocatable)
#define __T()_declare_result __IDENTITY(__T()_declare(__T()_kindlen_component__))__IDENTITY(__T_dimension_result)
#define __T()_declare_dummy __IDENTITY(__T()_declare(__T()_kindlen_dummy__))__IDENTITY(__T_dimension_dummy)

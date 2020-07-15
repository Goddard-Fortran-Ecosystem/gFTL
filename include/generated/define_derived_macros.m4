changecom()
define(__T,`__`'_T()')

   ! Input tokens
   !   Mandatory:
   !     __T
   !
   !   Optional:
   ! 
   !     __T()_deferred
   !     __T()_polymorphic
   !     __T()_KINDLEN
   !     __T()_kindlen_dummy
   !     __T()_kindlen_component
   !     __T()_rank
   !     __T()_shape

   !     __T()_EQ
   !     __T()_NE
   !     __T()_LT
   !     __T()_GT
   !     __T()_LE
   !     __T()_GE

   !     __T()_EQ_SCALAR
   !     __T()_NE_SCALAR
   !     __T()_LT_SCALAR
   !     __T()_GT_SCALAR
   !     __T()_LE_SCALAR
   !     __T()_GE_SCALAR

   !     __T()_MOVE
   !     __T()_FREE
   !     __T()_COPY

   !  Output tokens:  (some not defined depending upon inputs)
   !
   !     __T()_declare_component__
   !     __T()_declare_result__
   !     __T()_declare_dummy__

   !     __T()_DECLARE__
   !     __T()_allocatable__
   !     __T()_deferred__
   !     __T()_EQ_SCALAR__
   !     __T()_NE_SCALAR__
   !     __T()_LT_SCALAR__
   !     __T()_GT_SCALAR__
   !     __T()_LE_SCALAR__
   !     __T()_GE_SCALAR__

   !     __T()_KINDLEN__
   !     __T()_kindlen_dummy__
   !     __T()_kindlen_component__

   !     __T()_dimensions_result__
   !     __T()_dimensions_component__
   !     __T()_dimensions_dummy__



#if __T() > 0
   ! Instrinsic types

   ! Some older compilers do not support declaring intrinsics with TYPE,
   ! e.g., TYPE(integer), so we suppress that here.

   ! Special per-type settings for different intrinsics:

#    if __T() == __COMPLEX

#        define __T()_DECLARE__(kindlen) __IDENTITY(complex)__IDENTITY(kindlen)
#        define __T()_EQ_SCALAR__(a,b) a == b
#        define __T()_name__ "complex"

#    elif __T() == __LOGICAL

#        define __T()_DECLARE__(kindlen) __IDENTITY(logical)__IDENTITY(kindlen)
#        define __T()_EQ_SCALAR__(a,b) a .eqv. b
#        define __T()_NE_SCALAR__(a,b) a .neqv. b
#        define __T()_name__ "logical"

#    else

!        Most intrinsics can be compared with consistent scalars
#        define __T()_EQ_SCALAR__(a,b) a == b
#        define __T()_NE_SCALAR__(a,b) a /= b
#        define __T()_LE_SCALAR__(a,b) a <= b
#        define __T()_GE_SCALAR__(a,b) a >= b
#        define __T()_LT_SCALAR__(a,b) a <  b
#        define __T()_GT_SCALAR__(a,b) a >  b

#        if __T() == __INTEGER

#            define __T()_DECLARE__(kindlen) __IDENTITY(integer)__IDENTITY(kindlen)
#            define __T()_name__ "integer"

#        elif __T() == __REAL

#            define __T()_DECLARE__(kindlen) __IDENTITY(real)__IDENTITY(kindlen)
#            define __T()_name__ "real"

#        elif __T() == __CHARACTER

#            define __T()_DECLARE__(kindlen) __IDENTITY(character)__IDENTITY(kindlen)
#            ifdef __T()_deferred
#                define __T()_KINDLEN__(context) (len=context)
#            endif
#            define __T()_name__ "character"

#        endif

#    endif
#else
! User defined derived type (or C_PTR, etc)
#    if  __T()_polymorphic
#        define __T()_DECLARE__(t) class(t)
#    else
#        define __T()_DECLARE__(t) type(t)
#    endif
#endif

#ifdef __T()_KINDLEN
#    define __T()_KINDLEN__(context)  __T()_KINDLEN(context)
#else
#    define __T()_KINDLEN__(context)
#endif

#ifdef __T()_kindlen_dummy
#    define __T()_kindlen_dummy__ __T()_kindlen_dummy
#else
#    define __T()_kindlen_dummy__ __T()_KINDLEN__(*)
#endif

#ifdef __T()_kindlen_component
#    define __T()_kindlen_component__ __T()_kindlen_component
#else
#    define __T()_kindlen_component__ __T()_KINDLEN__(:)
#endif

#ifdef __T()_kindlen_string
#    define __T()_kindlen_string__ //__T()_kindlen_string
#else
#    define __T()_kindlen_string__
#endif

#ifdef __T()_rank
#    if __T()_rank == 1
#        define __T()_dimension_result__ , dimension(:)
#        define __T()_rank_string__ "1"
#        define __T()_dimension_string__ ", dimension(:)"
#    elif __T()_rank == 2
#        define __T()_dimension_result__ , dimension(:,:)
#        define __T()_rank_string__ "2"
#        define __T()_dimension_string__ ", dimension(:,:)"
#    elif __T()_rank == 3
#        define __T()_dimension_result__ , dimension(:,:,:)
#        define __T()_rank_string__ "3"
#        define __T()_dimension_string__ ", dimension(:,:,:)"
#    elif __T()_rank == 4
#        define __T()_dimension_result__ , dimension(:,:,:,:)
#        define __T()_rank_string__ "4"
#        define __T()_dimension_string__ ", dimension(:,:,:,:)"
#    elif __T()_rank == 5
#        define __T()_dimension_result__ , dimension(:,:,:,:,:)
#        define __T()_rank_string__ "5"
#        define __T()_dimension_string__ ", dimension(:,:,:,:,:)"
#    elif __T()_rank == 6
#        define __T()_dimension_result__ , dimension(:,:,:,:,:,:)
#        define __T()_rank_string__ "6"
#        define __T()_dimension_string__ ", dimension(:,:,:,:,:,:)"
#    elif __T()_rank == 7
#        define __T()_dimension_result__ , dimension(:,:,:,:,:,:,:)
#        define __T()_rank_string__ "7"
#        define __T()_dimension_string__ ", dimension(:,:,:,:,:,:,:)"
#    endif
#    if defined(__T()_shape)
         ! fixed shape       
#        define __T()_dimension_component__ , __IDENTITY(dimension)__IDENTITY(__T()_shape)
#        ifdef __T()_shape_string
#            define __T()_dimension_string__ , ", dimension("//__T()_shape_string//")"
#        else
#            define __T()_dimension_string__ , ", dimension(rank<"//"T_()_rank_string__//">)"
#        endif
#    else
#        ! assumed shape
#        define __T()_dimension_component__ __T()_dimension_result
#        define __T()_dimension_string__ ", dimension("__T()_shape_string__//")"
#    endif
#    define __T()_dimension_dummy __T()_dimension_component
#
#else
! Scalar
#    define __T()_dimension_result__
#    define __T()_dimension_component__
#    define __T()_dimension_dummy__
#    define __T()_dimension_string__
#endif

#if defined(__T()_deferred) || defined(__T()_polymorphic) || (defined(__T()_rank) && !defined(__T()_shape))
#   define __T()_allocatable__
#   define __T()_allocatable_attr_ , allocatable
#   define __T()_allocatable_string__ ", allocatable"
#else
#   define __T()_allocatable_attr__
#   define __T()_allocatable_string__
#endif

#define __T()_declare_component__ __IDENTITY(__T()_DECLARE__(__T()_kindlen_component__))__IDENTITY(__T()_dimension_component__)__IDENTITY(__T()_allocatable_attr__)
#define __T()_declare_result__ __IDENTITY(__T()_DECLARE__(__T()_kindlen_component__))__IDENTITY(__T()_dimension_result__)
#define __T()_declare_dummy__ __IDENTITY(__T()_DECLARE__(__T()_kindlen_dummy__))__IDENTITY(__T()_dimension_dummy__)

#ifdef  __T()_name
#    define __T()_name__ __T()_name
#endif
#ifdef __T()_name__
#    define __T()_declare_string__ __IDENTITY(__T()_name__)__IDENTITY(__T()_kindlen_string__)__IDENTITY(__T()_dimension_string__)__IDENTITY(__T()_allocatable_string__)
#endif




#ifdef __T()_FREE
#    define __T()_FREE__(x)  __T()_FREE(x)
#else
#    ifdef __T()_allocatable__
#        define __T()_FREE__(x)  deallocate(x)
#    else
#        define __T()_FREE__(x)
#    endif
#endif

#ifdef __T()_MOVE
#    define  __T()_MOVE__(dst,src)  __T()_MOVE(dst,src)
#else
#    ifdef __T()_allocatable__
#        define  __T()_MOVE__(dst,src)  call move_alloc(from=src,to=dst)
#    else
#        define  __T()_MOVE__(dst,src)  dst=src
#    endif
#endif


#ifdef __T()_COPY
#    define  __T()_COPY__(dst,src)  __T()_COPY(dst,src)
#else
#    define __T()_COPY__(dst,src) dst=src
#endif

  

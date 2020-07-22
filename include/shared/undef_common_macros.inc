#ifdef __gftl_common
#    undef __gftl_common
#endif

! Per-container name mangling   
#ifdef __IDENTITY
#    undef __IDENTITY
#endif

#ifdef __GUARD
#    undef __GUARD
#endif


! Fortran portability support
#ifdef __gftl_impure_elemental__
#    undef __gftl_impure_elemental__
#endif

#ifdef __gftl_size_kind
#    undef __gftl_size_kind
#endif
#ifdef __gftl_disable_index_kind_overload
#    undef __gftl_disable_index_kind_overload
#endif


! Predefined intrinsic types

#ifdef __LOGICAL__
#    undef __LOGICAL__
#endif
#ifdef __INTEGER_
#    undef __INTEGER__
#endif
#ifdef __REAL__
#    undef __REAL__
#endif
#ifdef __COMPLEX__
#    undef __COMPLEX__
#endif
#ifdef __CHARACTER__
#    undef __CHARACTER__
#endif
#ifdef __DOUBLE_PRECISION__
#    undef __DOUBLE_PRECISION__
#endif


#ifdef __LOGICAL
#    undef __LOGICAL
#endif
#ifdef __INTEGER
#    undef __INTEGER
#endif
#ifdef __INTEGER8
#    undef __INTEGER8
#endif
#ifdef __INTEGER16
#    undef __INTEGER16
#endif
#ifdef __INTEGER32
#    undef __INTEGER32
#endif
#ifdef __INTEGER64
#    undef __INTEGER64
#endif

#ifdef __REAL
#    undef __REAL
#endif
#ifdef __REAL16
#    undef __REAL16
#endif
#ifdef __REAL32
#    undef __REAL32
#endif
#ifdef __REAL64
#    undef __REAL64
#endif
#ifdef __REAL128
#    undef __REAL128
#endif
   
#ifdef __COMPLEX
#    undef __COMPLEX
#endif
#ifdef __COMPLEX16
#    undef __COMPLEX16
#endif
#ifdef __COMPLEX32
#    undef __COMPLEX32
#endif
#ifdef __COMPLEX64
#    undef __COMPLEX64
#endif
#ifdef __COMPLEX128
#    undef __COMPLEX128
#endif
   
#ifdef __CHARACTER
#    undef __CHARACTER
#endif
#ifdef __CHARACTER_DEFERRED
#    undef __CHARACTER_DEFERRED
#endif

#ifdef __DOUBLE_PRECISION
#    undef __DOUBLE_PRECISION
#endif

#ifdef __POLYMORPHIC
#    undef __POLYMORPHIC
#endif
#ifdef __DEFERRED
#    undef __DEFERRED
#endif



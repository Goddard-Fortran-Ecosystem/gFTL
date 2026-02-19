changecom()


define(`cpp_copy',`
#ifdef _T_in()_$1
! define _T_out()_$1
ifelse($#,1,
#    define _T_out()_$1 _T_in()_$1,
$#,2,
#    define _T_out()_$1($2) _T_in()_$1($2),
#    define _T_out()_$1($2,$3) _T_in()_$1($2,$3))
#endif'

)

#ifdef _T_out()
#    undef _T_out()
#endif
#define _T_out() _T_in()

#ifdef _T_out()_name
#    undef _T_out()_name
#endif
cpp_copy(name)

#ifdef _T_out()_is_intrinsic
#    undef _T_out()_is_intrinsic
#endif
cpp_copy(is_intrinsic)

#ifdef _T_out()_string
#    undef _T_out()_string
#endif
cpp_copy(string)

#ifdef _T_out()_EQ_SCALAR
#    undef _T_out()_EQ_SCALAR
#endif
cpp_copy(EQ_SCALAR,a,b)

#ifdef _T_out()_NE_SCALAR
#    undef _T_out()_NE_SCALAR
#endif
cpp_copy(NE_SCALAR,a,b)

#ifdef _T_out()_LE_SCALAR
#    undef _T_out()_LE_SCALAR
#endif
cpp_copy(LE_SCALAR,a,b)

#ifdef _T_out()_GE_SCALAR
#    undef _T_out()_GE_SCALAR
#endif
cpp_copy(GE_SCALAR,a,b)

#ifdef _T_out()_LT_SCALAR
#    undef _T_out()_LT_SCALAR
#endif
cpp_copy(LT_SCALAR,a,b)

#ifdef _T_out()_GT_SCALAR
#    undef _T_out()_GT_SCALAR
#endif
cpp_copy(GT_SCALAR,a,b)

#ifdef _T_out()_KINDLEN
#    undef _T_out()_KINDLEN
#endif
cpp_copy(KINDLEN,context)

#ifdef _T_out()_kindlen_dummy
#    undef _T_out()_kindlen_dummy
#endif
cpp_copy(kindlen_dummy)

#ifdef _T_out()_kindlen_component
#    undef _T_out()_kindlen_component
#endif
cpp_copy(kindlen_component)

#ifdef _T_out()_kindlen_string
#    undef _T_out()_kindlen_string
#endif
cpp_copy(kindlen_string)

#ifdef _T_out()_default
#    undef _T_out()_default
#endif
cpp_copy(default)

#ifdef _T_out()_rank
#    undef _T_out()_rank
#endif
cpp_copy(rank)

#ifdef _T_out()_shape
#    undef _T_out()_shape
#endif
cpp_copy(shape)

#ifdef _T_out()_polymorphic
#    undef _T_out()_polymorphic
#endif
cpp_copy(polymorphic)

#ifdef _T_out()_deferred
#    undef _T_out()_deferred
#endif
cpp_copy(deferred)

#ifdef _T_out()_FREE
#    undef _T_out()_FREE
#endif
cpp_copy(FREE,x)

#ifdef _T_out()_COPY
#    undef _T_out()_COPY
#endif
cpp_copy(COPY,lhs,rhs)

#ifdef _T_out()_MOVE
#    undef _T_out()_MOVE
#endif
cpp_copy(MOVE,lhs,rhs)

#ifdef _T_out()_LT
#    undef _T_out()_LT
#endif
cpp_copy(LT,lhs,rhs)

#ifdef _T_out()_EQ
#    undef _T_out()_EQ
#endif
cpp_copy(EQ,lhs,rhs)

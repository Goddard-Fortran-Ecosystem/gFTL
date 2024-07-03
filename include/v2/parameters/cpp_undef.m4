changecom()

define(__T,`__`'_T()')
define(`cpp_undef',`
#ifdef __T()_$1
#    undef __T()_$1
#endif'
)

#ifdef __T()
#    undef __T()
#endif

cpp_undef(name)

cpp_undef(is_intrinsic)
cpp_undef(string)

cpp_undef(EQ_SCALAR,a,b)
cpp_undef(NE_SCALAR,a,b)
cpp_undef(LE_SCALAR,a,b)
cpp_undef(GE_SCALAR,a,b)
cpp_undef(LT_SCALAR,a,b)
cpp_undef(GT_SCALAR,a,b)

cpp_undef(KINDLEN,context)
cpp_undef(kindlen_dummy)
cpp_undef(kindlen_component)
cpp_undef(kindlen_string)

cpp_undef(default)

cpp_undef(rank)
cpp_undef(shape)
cpp_undef(extents)
cpp_undef(polymorphic)
cpp_undef(deferred)

cpp_undef(FREE)
cpp_undef(COPY)
cpp_undef(MOVE)

cpp_undef(LT,lhs,rhs)
cpp_undef(EQ,lhs,rhs)

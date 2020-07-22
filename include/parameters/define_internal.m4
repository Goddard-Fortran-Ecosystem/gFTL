changecom()

define(__T,`__`'_T()')

define(`cpp_copy',`
#ifdef _T()_$1
ifelse($#,1,
#    define __T()_$1 _T()_$1,
$#,2,
#    define __T()_$1($2) _T()_$1($2),
#    define __T()_$1($2,$3) _T()_$1($2,$3))
#endif'
)

#define __T() _T()
cpp_copy(is_intrinsic)
cpp_copy(string)

cpp_copy(EQ_SCALAR,a,b)
cpp_copy(NE_SCALAR,a,b)
cpp_copy(LE_SCALAR,a,b)
cpp_copy(GE_SCALAR,a,b)
cpp_copy(LT_SCALAR,a,b)
cpp_copy(GT_SCALAR,a,b)

cpp_copy(KINDLEN,context)
cpp_copy(kindlen_dummy)
cpp_copy(kindlen_component)
cpp_copy(kindlen_string)

cpp_copy(rank)
cpp_copy(extents)

cpp_copy(FREE,x)
cpp_copy(COPY,dst,src)
cpp_copy(MOVE,dst,src)


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

#define _T_out() _T_in()

cpp_copy(name)

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

cpp_copy(default)

cpp_copy(rank)
cpp_copy(shape)
cpp_copy(polymorphic)
cpp_copy(deferred)

cpp_copy(FREE,x)
cpp_copy(COPY,lhs,rhs)
cpp_copy(MOVE,lhs,rhs)

cpp_copy(LT,lhs,rhs)
cpp_copy(EQ,lhs,rhs)

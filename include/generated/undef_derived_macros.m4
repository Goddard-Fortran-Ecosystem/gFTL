changecom()
define(__T,`__`'_T()')
define(`cpp_undef',`
#ifdef __T()$1
#  undef __T()$1
#endif'
)

cpp_undef(declare)

! Relational operators
cpp_undef(eq)
cpp_undef(ne)
cpp_undef(le)
cpp_undef(ge)
cpp_undef(lt)
cpp_undef(gt)

cpp_undef(kindlen__)
cpp_undef(kindlen_component__)
cpp_undef(kindlen_dummy__)

cpp_undef(declare_component)
cpp_undef(declare_result)
cpp_undef(declare_dummy)

cpp_undef(alocatable)




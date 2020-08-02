changecom()
define(__T,`__`'_T()')
define(`cpp_undef',`
#ifdef __T()_$1
#  undef __T()_$1
#endif'
)

cpp_undef(declare)

! Relational operators
cpp_undef(EQ)
cpp_undef(NE)
cpp_undef(LE)
cpp_undef(GE)
cpp_undef(LT)
cpp_undef(GT)

cpp_undef(KINDLEN__)
cpp_undef(kindlen_component__)
cpp_undef(kindlen_dummy__)

cpp_undef(declare_component)
cpp_undef(declare_result)
cpp_undef(declare_dummy)

cpp_undef(allocatable)
cpp_undef(listable)

cpp_undef(EQ)
cpp_undef(LT)

cpp_undef(FREE)
cpp_undef(MOVE)
cpp_undef(COPY)



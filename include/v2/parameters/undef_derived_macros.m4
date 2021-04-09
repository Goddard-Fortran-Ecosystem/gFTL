changecom()
define(__T,`__`'_T()')
define(`cpp_undef',`
#ifdef __T()_$1
#  undef __T()_$1
#endif'
)

cpp_undef(declare)

cpp_undef(type__)
cpp_undef(name__)
cpp_undef(DECLARE__)
cpp_undef(NAME__)

! Relational operators
cpp_undef(EQ_SCALAR__)
cpp_undef(NE_SCALAR__)
cpp_undef(LE_SCALAR__)
cpp_undef(GE_SCALAR__)
cpp_undef(LT_SCALAR__)
cpp_undef(GT_SCALAR__)

cpp_undef(KINDLEN__)
cpp_undef(kindlen_component__)
cpp_undef(kindlen_dummy__)
cpp_undef(kindlen_string__)

cpp_undef(dimension_string__)
cpp_undef(dimension_result__)
cpp_undef(dimension_component__)
cpp_undef(dimension_dummy__)

cpp_undef(declare_component__)
cpp_undef(declare_result__)
cpp_undef(declare_dummy__)

cpp_undef(polymorphic__)
cpp_undef(deferred__)
cpp_undef(allocatable__)
cpp_undef(allocatable_attr__)
cpp_undef(allocatable_string__)
cpp_undef(listable__)

cpp_undef(default__)
cpp_undef(default_scalar__)
cpp_undef(rank__)
cpp_undef(shape__)

cpp_undef(EQ__)
cpp_undef(LT__)

cpp_undef(FREE__)
cpp_undef(MOVE__)
cpp_undef(COPY__)



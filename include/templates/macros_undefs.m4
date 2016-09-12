include(header.m4)

define(`cpp_undef',
#ifdef `$1'
#  undef `$1'
#endif
)

cpp_undef(_param())
cpp_undef(_base()_rank)
cpp_undef(_base()_extents)
cpp_undef(_base()_string)
cpp_undef(_base()_string_deferred)
cpp_undef(_base()_logical)
cpp_undef(_base()_pointer)
cpp_undef(_base()_allocatable)
cpp_undef(_base()_procedure)

cpp_undef(_BASE()_EQ)
cpp_undef(_BASE()_EQ_ELEMENT)
cpp_undef(_BASE()_LESS_THAN)
cpp_undef(_BASE()_ASSIGN)
cpp_undef(_BASE()_MOVE)
cpp_undef(_BASE()_FREE)


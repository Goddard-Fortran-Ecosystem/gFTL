
#include "parameters/T1/undef_pair_T1.inc"
#include "parameters/T2/undef_pair_T2.inc"

#ifdef __pair
#    undef __pair
#endif
#ifdef __pair_guard
#    undef __pair_guard
#endif

#include "shared/undef_common_macros.inc"

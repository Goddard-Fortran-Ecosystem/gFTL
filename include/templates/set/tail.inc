#include "parameters/T/undef_set_T.inc"

#ifdef __set
#    undef __set
#endif
#ifdef __set_iterator
#    undef __set_iterator
#endif
!!$#ifdef __set_riterator
!!$#    undef __set_riterator
!!$#endif

#ifdef __set_guard
#    undef __set_guard
#endif

#include "shared/undef_common_macros.inc"

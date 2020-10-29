#include "parameters/Key/undef_map_Key.inc"
#include "parameters/T/undef_map_T.inc"

#ifdef __map
#    undef __map
#endif
#ifdef __map_iterator
#    undef __map_iterator
#endif
!!$#ifdef __map_riterator
!!$#    undef __map_riterator
!!$#endif
#ifdef __map_pair
#  undef __map_pair
#endif
#ifdef __map_guard
#    undef __map_guard
#endif

#include "shared/undef_common_macros.inc"

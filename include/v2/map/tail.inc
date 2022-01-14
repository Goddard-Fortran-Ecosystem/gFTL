#include "parameters/Key/undef_map_Key.inc"
#include "parameters/T/undef_map_T.inc"

#ifdef __map
#    undef __map
#endif
#ifdef __map_constructor
#    undef __map_constructor
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

#ifdef __map_set
#  undef __map_set
#endif
#ifdef __map_set_constructor
#  undef __map_set_constructor
#endif
#ifdef __map_set_node
#  undef __map_set_node
#endif
#ifdef __map_set_iterator
#  undef __map_set_iterator
#endif

#ifdef __map_guard
#    undef __map_guard
#endif

#include "shared/undef_common_macros.inc"

#include "parameters/Key/undef_omap_Key.inc"
#include "parameters/T/undef_omap_T.inc"

#ifdef __omap
#    undef __omap
#endif
#ifdef __omap_iterator
#    undef __omap_iterator
#endif
!!$#ifdef __map_riterator
!!$#    undef __map_riterator
!!$#endif

#ifdef __omap_pair
#  undef __omap_pair
#endif

#ifdef __omap_set
#  undef __omap_set
#endif
#ifdef __omap_set_node
#  undef __omap_set_node
#endif
#ifdef __omap_set_iterator
#  undef __omap_set_iterator
#endif


#ifdef __omap_guard
#    undef __omap_guard
#endif

#include "shared/undef_common_macros.inc"

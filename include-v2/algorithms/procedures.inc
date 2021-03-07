#include "parameters/T/copy_algorithm_T_to_internal_T.inc"
#include "parameters/T/define_derived_macros.inc"

#define __guard __algorithm_guard
#define __iterator __algorithm_iterator
#include "algorithms/dir_procedures.inc"
#undef __iterator
#undef __guard
   

#ifdef __algorithm_riterator
#  define __iterator __algorithm_riterator
#  define __guard __IDENTITY(__algorithm_guard)reverse_
#  include "algorithms/dir_procedures.inc"
#  undef __guard
#  undef __iterator
#endif

#include "parameters/T/undef_derived_macros.inc"
   

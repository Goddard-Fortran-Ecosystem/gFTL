#define __algorithm_guard forward_
#include "algorithms/dir_procedures.inc"
   #undef __algorithm_guard

#ifdef __reverse_iterator
#    define  __algorithm_guard reverse_
#    include "algorithms/dir_procedures.inc"
#    undef __algorithm_guard
#endif

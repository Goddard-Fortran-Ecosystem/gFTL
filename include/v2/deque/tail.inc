
#include "parameters/T/undef_deque_T.inc"

#ifdef __deque
#    undef __deque
#endif
#ifdef __deque_iterator
#    undef __deque_iterator
#endif
#ifdef __deque_riterator
#    undef __deque_riterator
#endif
#ifdef __deque_guard
#    undef __deque_guard
#endif
#ifdef __deque_bucket_size
#    undef __deque_bucket_size
#endif

#include "shared/undef_common_macros.inc"

#include "shared/define_common_macros.inc"
#include "parameters/T/copy_T_to_queue_T.inc"

#define __queue Queue
#if defined(queue_container_type)
#    define __queue_container_type queue_container_type
#else
#    define __queue_container_type Deque
#endif
   
   
#define __queue_guard queue_


#include "shared/define_common_macros.inc"
#include "parameters/T/copy_T_to_stack_T.inc"

#define __stack Stack
#if defined(stack_container_type)
#    define __stack_container_type stack_container_type
#else
#    define __stack_container_type Deque
#endif
   
   
#define __stack_guard stack_


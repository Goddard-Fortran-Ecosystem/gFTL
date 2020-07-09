#define __T __INTEGER
#define __T_kindlen(context) (kind=INT32)
#define __T_rank 2
#define __T_shape (3,5)

#include "shared/define_common_macros.inc"
#include "generated/define_derived_macros_for_T.inc"

__T_declare_component
__T_declare_result
__T_declare_dummy

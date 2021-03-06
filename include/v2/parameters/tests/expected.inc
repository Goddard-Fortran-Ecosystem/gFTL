#include "shared/define_common_macros.inc"
#include "parameters/T/define_derived_macros.inc"
<start>
declare string:    __T_declare_string__
declare component: __T_declare_component__
declare result:    __T_declare_result__
declare dummy:     __T_declare_dummy__

a == b:            __T_EQ_SCALAR__(a,b)
a NE b:            __T_NE_SCALAR__(a,b)
a <= b:            __T_LE_SCALAR__(a,b)
a >= b:            __T_GE_SCALAR__(a,b)
a <  b:            __T_LT_SCALAR__(a,b)
a >  b:            __T_GT_SCALAR__(a,b)

FREE(x):           __T_FREE__(x)
COPY(dst,src):     __T_COPY__(dst,src)
MOVE(dst,src):     __T_MOVE__(dst,src)
<stop>

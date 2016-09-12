changecom()
define(_param,_`'param())
define(__param,__`'param())
define(_PARAM,_`'translit(param(),`a-z',`A-Z'))
define(__PARAM,__`'translit(param(),`a-z',`A-Z'))
ifelse(param,type,`define(`_base',)',`define(`_base',_param())')
ifelse(param,type,`define(`_BASE',)',`define(`_BASE',_PARAM())')

ifelse(param,type,`define(`_use',__use)',`define(`_use',__use`'_param())')
ifelse(param,type,`define(`_USE',__USE)',`define(`_USE',__USE`'_PARAM())')

define(default,`ifelse(`$1',,`_param()',)')

define(`use_define',
#ifdef _base()`$1'
#  define _use()`$1' _base()`$1'
#endif
)

define(`USE_define',
#ifdef _BASE()`$1'
#  define _USE()`$1'`$2' _BASE()`$1'`$2'
#endif
)


use_define(default(_base()))
use_define(_rank)
use_define(_extents)
use_define(_string)
use_define(_string_deferred)
use_define(_logical)
use_define(_pointer)
use_define(_allocatable)
use_define(_procedure)

use_define(_equal_defined)
use_define(_less_than_defined)

USE_define(_ASSIGN,(dest,src))
USE_define(_MOVE,(dest,src))
USE_define(_FREE,(x))

USE_define(_LESS_THAN,(x,y))
USE_define(_EQ_ELEMENT,(x,y))
USE_define(_EQ,(x,y))

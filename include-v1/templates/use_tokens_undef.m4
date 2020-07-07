!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

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

define(`use_undef',
#ifdef _use()`$1'
#  undef _use()`$1'
#endif
)

define(`USE_undef',
#ifdef _USE()`$1'
#  undef _USE()`$1'
#endif
)


use_undef(default(_base()))
use_undef(_rank)
use_undef(_extents)
use_undef(_string)
use_undef(_string_deferred)
use_undef(_logical)
use_undef(_pointer)
use_undef(_allocatable)
use_undef(_procedure)

use_undef(_equal_defined)
use_undef(_less_than_defined)

USE_undef(_ASSIGN)
USE_undef(_MOVE)
USE_undef(_FREE)

USE_undef(_LESS_THAN)
USE_undef(_EQ_ELEMENT)
USE_undef(_EQ)


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

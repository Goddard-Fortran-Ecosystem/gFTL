!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

changecom()
define(_param,__use_`'param())
define(__param,__`'param())
define(_PARAM,__use_`'translit(param(),`a-z',`A-Z'))
define(__PARAM,__`'translit(param(),`a-z',`A-Z'))
ifelse(param,type,`define(`_base',__use)',`define(`_base',_param())')
ifelse(param,type,`define(`_BASE',__USE)',`define(`_BASE',_PARAM())')

#undef __param()_target_type
#undef __param()_rank
#undef __param()_deferred_dim_attr
#undef __param()_dimension_attr
#undef __param()_target_attrs
#undef __param()_dummy_attrs
#undef __param()_result_attrs

#undef __param()_wrapped
#undef __param()_declare_type
#undef __param()_declare_target
#undef __param()_declare_dummy
#undef __param()_declare_result
#undef __param()_declare_element_type
#undef __param()_component_attrs
#undef __param()_interface

#undef __PARAM()_ASSIGN
#undef __PARAM()_MOVE
#undef __PARAM()_FREE
#undef __PARAM()_EQ
#undef __PARAM()_EQ_ELEMENT

#undef __PARAM()_LESS_THAN
#undef __param()_needs_default_compare
#undef __param()_compare_well_defined


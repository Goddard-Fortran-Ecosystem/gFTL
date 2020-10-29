!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

#include "templates/key_template_macros.inc"
#include "templates/value_template_macros.inc"

      type :: __pair
        __key_declare_component :: key
        __value_declare_component :: value
      contains
        procedure :: pairEqual
        generic :: operator(==) => pairEqual
        procedure :: pairSameKey
        generic :: operator(.sameKey.) => pairSameKey
      end type __pair

      interface __pair
         module procedure __PROC(newPair)
      end interface __pair
#include "templates/key_template_macros_undefs.inc"
#include "templates/value_template_macros_undefs.inc"

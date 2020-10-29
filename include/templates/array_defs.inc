!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

! Must define:
!    __array_declare_dummy
!    __ARRAY_EQ_ELEMENT

      logical function __PROC(eqArray)(x, y) result(equal)
      __array_declare_dummy, intent(in) :: x
      __array_declare_dummy, intent(in) :: y

      if (.not. all(shape(x) == shape(y))) then
         equal = .false.
         return
      end if

      equal = all(__ARRAY_EQ_ELEMENT(x,y))

      end function __PROC(eqArray)

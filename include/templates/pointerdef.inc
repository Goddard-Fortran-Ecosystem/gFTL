!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

! Encompassing code must set define the following tokens/Macros:
! __pointer_declare_result
! __pointer_declare_dummy

      function __PROC(sameptr)(x, y) result(same)
      __pointer_declare_result, pointer, intent(in) :: x
      __pointer_declare_dummy :: y
      logical :: same
      __pointer_declare_result, pointer :: p

      p => y
      same = (.not. associated(x) .and. .not. associated(p)) .or.            &
     &       associated(x, p)

      end function __PROC(sameptr)
#undef __need_compare

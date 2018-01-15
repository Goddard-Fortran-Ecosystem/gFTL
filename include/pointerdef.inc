!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

      function addr(x)
      implicit none
      _type, pointer, intent(in) :: x
      integer :: addr

      addr=loc(x)
      return
      end function addr

      pure function sameptr(x, y)
      implicit none
      _type, pointer, intent(in) :: x
      _type, target, intent(in) :: y
      logical :: sameptr

      sameptr=(.not.associated(x)) .or. associated(x, y)
      return
      end function sameptr
#undef _type

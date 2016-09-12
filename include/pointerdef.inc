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

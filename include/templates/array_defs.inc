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

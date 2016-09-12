module  CIStringIntegerMap_mod
#define _key_string_deferred
#define _key_equal_defined
#define _KEY_LESS_THAN(x,y) caseInsensitiveLessThan(x,y)

#define _value integer
#define _value_equal_defined
#define _value_less_than_defined


#include "templates/map.inc"


   logical function caseInsensitiveLessThan(x,y) result(less)
      character(len=*), intent(in) :: x
      character(len=*), intent(in) :: y

      integer :: i
      character(1) :: cx, cy

      integer, parameter :: UPPER_LOWER_DELTA = iachar('A') - iachar('a')
      
      do i = 1, min(len(x),len(y))
         cx = x(i:i)
         cy = y(i:i)

         if (cx >= 'A' .and. cx <= 'Z') then
            cx = achar(iachar(cx) - UPPER_LOWER_DELTA)
         end if

         if (cy >= 'A' .and. cy <= 'Z') then
            cy = achar(iachar(cy) - UPPER_LOWER_DELTA)
         end if

         less = (cx < cy)
         if (cx /= cy) then
            return
         end if

      end do

      less = (len(x) < len(y))

   end function caseInsensitiveLessThan

end module CIStringIntegerMap_mod


program main
   use CIStringIntegerMap_mod
   implicit none

   type (Map) :: m

   call m%insert('cat', 1)
   call m%insert('dog', 2)
   call m%insert('fish', 3)

   call check('cat', 1)
   call check('dog', 2)
   call check('fish', 3)

   call check('CAT', 1)
   call check('Cat', 1)
   call check('caT', 1)

contains

   subroutine check(str, expected)
      character(len=*), intent(in) :: str
      integer, intent(in) :: expected

      print*,"m%at('",str,"')  = ",m%at(str),"(should be",expected,")"
   end subroutine check

end program main

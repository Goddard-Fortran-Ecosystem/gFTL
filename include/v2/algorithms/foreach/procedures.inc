   contains

      function for_each_(first, last, fn)
         type(__iterator), intent(in) :: first
         type(__iterator), intent(in) :: last
         interface
            subroutine fn(item)
               __declare_T__, intent(inout) :: item
            end subroutine fn
         end interface

         type(__iterator) :: iter

         iter = first
         do while (iter /= last)
            call fn(iter%at())
            call iter%next()
         end do

      end function for_each_

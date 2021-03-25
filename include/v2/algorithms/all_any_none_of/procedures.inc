
      logical function all_of_(first, last, p)
         type(__iterator), intent(in) :: first
         type(__iterator), intent(in) :: last
         interface
            logical function p(item)
               __declare_T__, intent(in) :: item
            end function p
         end interface

         all_of_ = (find_if_not(first, last, p) == last)
      end function all_of_


      logical function any_of_(first, last, p)
         type(__iterator), intent(in) :: first
         type(__iterator), intent(in) :: last
         interface
            logical function p(item)
               __declare_T__, intent(in) :: item
            end function p
         end interface

         all_of_ = (find_if(first, last, p) /= last)
      end function any_of_


      logical function none_of_(first, last, p)
         type(__iterator), intent(in) :: first
         type(__iterator), intent(in) :: last
         interface
            logical function p(item)
               __declare_T__, intent(in) :: item
            end function p
         end interface

         all_of_ = (find_if(first, last, p) == last)
      end function none_of_


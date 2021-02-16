      
      function find_basic(first, last, value) result(it)
         type(__find_iterator) :: it
         type(__find_iterator), intent(in) :: first
         type(__find_iterator), intent(in) :: last
         type(__find_value), intent(in) :: value
         

         type(__find_iterator) :: iter
         
         iter = first
         do while (iter /= last)
            if (iter%of() == value) return
            call iter%next()
         end do
         
         iter = last
      end function find_basic
      
      
#ifdef __find_predicate
      function find_if_(first, last, p) result(it)
         type(__find_iterator) :: it
         type(__find_iterator), intent(in) :: first
         type(__find_iterator), intent(in) :: last
         interface
            logical function p(item)
               __declare_T__, intent(in) :: item
            end function p
         end interface
         
         type(__find_iterator) :: iter
         
         iter = first
         do while (iter /= last)
            if (p(iter%at())) return
            call iter%next()
         end do
         
         iter = last
      end function find_if_

      
      function find_if_not_(first, last, q) result(it)
         type(__find_iterator) :: it
         type(__find_iterator), intent(in) :: first
         type(__find_iterator), intent(in) :: last
         interface
            logical function q(item)
               __declare_T__, intent(in) :: item
            end function q
         end interface
         
         type(__find_iterator) :: iter
         
         iter = first
         do while (iter /= last)
            if (.not. q(iter%at())) return
            call iter%next()
         end do
         
         iter = last
      end function find_if_not_

#endif      
   

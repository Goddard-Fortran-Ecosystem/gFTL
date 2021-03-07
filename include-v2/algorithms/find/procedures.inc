#ifdef __T_EQ__
      function __MANGLE(find_basic)(first, last, value) result(iter)
         type(__iterator) :: iter
         type(__iterator), intent(in) :: first
         type(__iterator), intent(in) :: last
         __T_declare_dummy__, intent(in) :: value
         

         iter = first
         do while (iter /= last)
            if (__T_EQ__(iter%of(),value)) return
            call iter%next()
         end do
         
         iter = last

      end function __MANGLE(find_basic)
#endif      
      
#ifdef __algorithm_predicate
      function __MANGLE(find_if)(first, last, p) result(it)
         type(__iterator) :: it
         type(__iterator), intent(in) :: first
         type(__iterator), intent(in) :: last
         interface
            logical function p(item)
               __T_declare_dummy__, intent(in) :: item
            end function p
         end interface
         
         type(__iterator) :: iter
         
         iter = first
         do while (iter /= last)
            if (p(iter%at())) return
            call iter%next()
         end do
         
         iter = last
      end function __MANGLE(find_if)

      
      function __MANGLE(find_if_not)(first, last, q) result(it)
         type(__iterator) :: it
         type(__iterator), intent(in) :: first
         type(__iterator), intent(in) :: last
         interface
            logical function q(item)
               __T_declare_dummy__, intent(in) :: item
            end function q
         end interface
         
         type(__iterator) :: iter
         
         iter = first
         do while (iter /= last)
            if (.not. q(iter%at())) return
            call iter%next()
         end do
         
         iter = last
      end function __MANGLE(find_if_not)

#endif      
   

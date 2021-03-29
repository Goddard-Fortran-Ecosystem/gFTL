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
      
!#ifdef __algorithm_predicate
      function __MANGLE(find_if)(first, last, p) result(it)
         type(__iterator) :: it
         type(__iterator), intent(in) :: first
         type(__iterator), intent(in) :: last
         interface
            logical function p(item)
               import
               implicit none
               __T_declare_dummy__, intent(in) :: item
            end function p
         end interface
         
         it = first
         do while (it /= last)
            if (p(it%of())) return

            call it%next()
         end do
         
         it = last
      end function __MANGLE(find_if)

      
      function __MANGLE(find_if_not)(first, last, q) result(it)
         type(__iterator) :: it
         type(__iterator), intent(in) :: first
         type(__iterator), intent(in) :: last
         interface
            logical function q(item)
               import
               implicit none
               __T_declare_dummy__, intent(in) :: item
            end function q
         end interface
         
         it = first
         do while (it /= last)
            if (.not. q(it%of())) return
            call it%next()
         end do
         
         it = last
      end function __MANGLE(find_if_not)

!#endif      
   

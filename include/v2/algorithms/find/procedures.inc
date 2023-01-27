#ifdef __T_EQ__
      function __MANGLE(find_basic)(first, last, value) result(iter)
         type(__iterator) :: iter
         type(__iterator), intent(in) :: first
         type(__iterator), intent(in) :: last
         __T_declare_dummy__, intent(in) :: value

         __T_declare_result__, pointer :: ptr
         
         iter = first
         do while (iter /= last)
            ptr => iter%of()
            if (__T_EQ__(ptr,value)) return
            call iter%next()
         end do
         
         iter = last

       end function __MANGLE(find_basic)
#else
       ! Stub function to satisfy need to have something for the public interface
       ! And has to involve iterator type to avoid ambiguity with reverse iterator
       ! and analogous construct for other containers.
       function __MANGLE(find_basic)(do_not_use,unused) result(j)
          type :: __MANGLE(keywordenforcer)
             integer :: placeholder
         end type __MANGLE(Keywordenforcer)
         type(__MANGLE(keywordenforcer)) :: j
         type(__iterator), intent(in) :: do_not_use
         type(keywordenforcer), intent(in) :: unused

         j%placeholder = -1
         __UNUSED_DUMMY(do_not_use)
         __UNUSED_DUMMY(unused)
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
   

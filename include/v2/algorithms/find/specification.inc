  interface find
      module procedure __MANGLE(find_basic)
   end interface find
   
!#ifdef __find_predicate
   interface find_if
      module procedure __MANGLE(find_if)
   end interface find_if

   interface find_if_not
      module procedure __MANGLE(find_if_not)
   end interface find_if_not
!#endif



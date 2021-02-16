   interface find
      module procedure find_forward
   end interface find

#ifdef __find_predicate
   interface find_if
      module procedure find_if_forward
   end interface find_if

   interface find_if_not
      module procedure find_if_not_forward
   end interface find_if_not
#endif


#ifdef __reverse_iterator
   interface find
      module procedure find_reverse
   end interface find

#    ifdef __find_predicate
   interface find_if_basic
      module procedure find_if_reverse
   end interface find_if

   interface find_if_not
      module procedure find_if_not_reverse
   end interface find_if_not
#    endif

#endif


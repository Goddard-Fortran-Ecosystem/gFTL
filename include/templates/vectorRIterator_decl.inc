      type __riterator
         private

         __declare_element_type, dimension(:), pointer :: elements
         integer(kind=SIZE_KIND) :: currentIndex = -1 ! intentionally invalid value

      contains
         
         procedure :: get => __PROC(riter_get)
         procedure :: next => __PROC(riter_next)
         procedure :: previous => __PROC(riter_previous)
         
         procedure :: __PROC(riter_atDefault)
         generic :: at => __PROC(riter_atDefault)
         procedure :: __PROC(riter_atOffset)
         generic :: at => __PROC(riter_atOffset)
#ifdef SUPPORT_FOR_INT64
         procedure :: __PROC(riter_atOffset_32)
         generic :: at => __PROC(riter_atOffset_32)
#endif
         
         
         procedure :: __PROC(riter_equal)
         procedure :: __PROC(riter_not_equal)
         generic :: operator(==) => __PROC(riter_equal)
         generic :: operator(/=) => __PROC(riter_not_equal)
         
         procedure :: __PROC(riter_less)
         procedure :: __PROC(riter_less_equal)
         procedure :: __PROC(riter_greater)
         procedure :: __PROC(riter_greater_equal)
         generic :: operator(<) => __PROC(riter_less)
         generic :: operator(<=) => __PROC(riter_less_equal)
         generic :: operator(>) => __PROC(riter_greater)
         generic :: operator(>=) => __PROC(riter_greater_equal)
         
         procedure :: __PROC(riter_add)
         procedure :: __PROC(riter_subtract)
         generic :: operator(+) => __PROC(riter_add)
         generic :: operator(-) => __PROC(riter_subtract)

#ifdef SUPPORT_FOR_INT64
         procedure :: __PROC(riter_add_32)
         procedure :: __PROC(riter_subtract_32)
         generic :: operator(+) => __PROC(riter_add_32)
         generic :: operator(-) => __PROC(riter_subtract_32)
#endif         
      end type __riterator
      


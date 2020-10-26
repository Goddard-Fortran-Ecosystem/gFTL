
   __type_declare_local :: ONE
   __type_declare_local :: ONE_B
   __type_declare_local :: TWO
   __type_declare_local :: THREE
   __type_declare_local :: FOUR
   __type_declare_local :: FIVE

#if defined(_pointer) & !defined(_procedure)

#  ifdef __type_allocatable_target
#    define __type_test_attrs , allocatable
#  else
#   define __type_test_attrs
#  endif

   __type_declare_target __type_test_attrs :: one_
   __type_declare_target __type_test_attrs :: one_b_
   __type_declare_target __type_test_attrs :: two_
   __type_declare_target __type_test_attrs :: three_
   __type_declare_target __type_test_attrs:: four_
   __type_declare_target __type_test_attrs :: five_

#  undef __type_test_attrs
#endif

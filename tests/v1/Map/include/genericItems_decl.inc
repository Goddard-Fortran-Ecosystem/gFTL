
   __value_declare_local :: ONE
   __value_declare_local :: TWO
   __value_declare_local :: THREE
   __value_declare_local :: FOUR
   __value_declare_local :: FIVE

#if defined(_value_pointer) & !defined(_value_procedure)

#  ifdef __value_allocatable_target
#    define __value_test_attrs , allocatable
#  else
#   define __value_test_attrs
#  endif

   __value_declare_target __value_test_attrs :: one_
   __value_declare_target __value_test_attrs :: two_
   __value_declare_target __value_test_attrs :: three_
   __value_declare_target __value_test_attrs:: four_
   __value_declare_target __value_test_attrs :: five_

#  undef __value_test_attrs
#endif



   __key_declare_local :: KEY1
   __key_declare_local :: KEY2
   __key_declare_local :: KEY3
   __key_declare_local :: KEY4
   __key_declare_local :: KEY5



#if defined(_key_pointer) & !defined(_key_procedure)

#  ifdef __key_allocatable_target
#    define __key_test_attrs , allocatable
#  else
#   define __key_test_attrs
#  endif

   __key_declare_target __key_test_attrs :: key1_
   __key_declare_target __key_test_attrs :: key2_
   __key_declare_target __key_test_attrs :: key3_
   __key_declare_target __key_test_attrs:: key4_
   __key_declare_target __key_test_attrs :: key5_

#  undef __key_test_attrs
#endif

! Vary _type
ADD_TEST_SUITE(Test_IntegeraltSet_mod_suite)
ADD_TEST_SUITE(Test_RealaltSet_mod_suite)
ADD_TEST_SUITE(Test_ComplexaltSet_mod_suite)

ADD_TEST_SUITE(Test_Real64altSet_mod_suite)

! Strings
#ifdef SUPPORTS_POINTERS_TO_FIXED_LENGTH_STRINGS
  ADD_TEST_SUITE(Test_Character17altSet_mod_suite) Test_Character17Set)
#endif
! workaround for gfortran 4.9.1
#ifdef SUPPORT_FOR_POINTERS_TO_DEFERRED_LENGTH_STRINGS
!ADD_TEST_SUITE(Test_DeferredLengthStringaltSet_mod_suite)
#endif

! pointers ...
ADD_TEST_SUITE(Test_IntegerAllocaltSet_mod_suite)
ADD_TEST_SUITE(Test_IntegerPtraltSet_mod_suite)

! arrays ...
ADD_TEST_SUITE(Test_Integer1daltSet_mod_suite)
ADD_TEST_SUITE(Test_Integer2daltSet_mod_suite)
ADD_TEST_SUITE(Test_Integer2dPtraltSet_mod_suite)
ADD_TEST_SUITE(Test_Integer2d_fixedExtentsaltSet_mod_suite)

! derived type
ADD_TEST_SUITE(Test_FooaltSet_mod_suite)
ADD_TEST_SUITE(Test_FooPtraltSet_mod_suite)
ADD_TEST_SUITE(Test_FooPolyaltSet_mod_suite)
ADD_TEST_SUITE(Test_FooPolyPtraltSet_mod_suite)

! Unlimited polymorphic
#ifndef __GFORTRAN__
!ADD_TEST_SUITE(Test_UnlimitedPolyaltSet_mod_suite) Test_UnlimitedPolyset)
#endif

ADD_TEST_SUITE(Test_unlimitedPolyPtraltSet_mod_suite)

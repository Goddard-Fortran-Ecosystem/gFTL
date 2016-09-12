! Vary _type
ADD_TEST_SUITE(Test_IntegerSet_mod_suite)
ADD_TEST_SUITE(Test_RealSet_mod_suite)
ADD_TEST_SUITE(Test_ComplexSet_mod_suite)

ADD_TEST_SUITE(Test_Real64Set_mod_suite)

! Strings
#ifdef SUPPORTS_POINTERS_TO_FIXED_LENGTH_STRINGS
  ADD_TEST_SUITE(Test_Character17Set_mod_suite) Test_Character17Set)
#endif
! workaround for gfortran 4.9.1
#ifdef SUPPORT_FOR_POINTERS_TO_DEFERRED_LENGTH_STRINGS
!ADD_TEST_SUITE(Test_DeferredLengthStringSet_mod_suite)
#endif

! pointers ...
ADD_TEST_SUITE(Test_IntegerAllocSet_mod_suite)
ADD_TEST_SUITE(Test_IntegerPtrSet_mod_suite)

! arrays ...
ADD_TEST_SUITE(Test_Integer1dSet_mod_suite)
ADD_TEST_SUITE(Test_Integer2dSet_mod_suite)
ADD_TEST_SUITE(Test_Integer2dPtrSet_mod_suite)
ADD_TEST_SUITE(Test_Integer2d_fixedExtentsSet_mod_suite)

! derived type
ADD_TEST_SUITE(Test_FooSet_mod_suite)
ADD_TEST_SUITE(Test_FooPtrSet_mod_suite)
ADD_TEST_SUITE(Test_FooPolySet_mod_suite)
ADD_TEST_SUITE(Test_FooPolyPtrSet_mod_suite)

! Unlimited polymorphic
#ifndef __GFORTRAN__
!ADD_TEST_SUITE(Test_UnlimitedPolySet_mod_suite) Test_UnlimitedPolyset)
#endif

ADD_TEST_SUITE(Test_unlimitedPolyPtrSet_mod_suite)

! Vary type
ADD_TEST_SUITE(Test_IntegerVector_mod_suite)
ADD_TEST_SUITE(Test_RealVector_mod_suite)
ADD_TEST_SUITE(Test_ComplexVector_mod_suite)
ADD_TEST_SUITE(Test_LogicalVector_mod_suite)

! Vary kind/len
ADD_TEST_SUITE(Test_Real64Vector_mod_suite)

! Vary attributes
ADD_TEST_SUITE(Test_IntegerPtrVector_mod_suite)
ADD_TEST_SUITE(Test_IntegerAllocVector_mod_suite)
ADD_TEST_SUITE(Test_Integer1dVector_mod_suite)
ADD_TEST_SUITE(Test_Integer2dVector_mod_suite)
ADD_TEST_SUITE(Test_Integer2d_fixedExtentsVector_mod_suite)
ADD_TEST_SUITE(Test_Integer2dPtrVector_mod_suite)

! Derived types
ADD_TEST_SUITE(Test_FooVector_mod_suite)
ADD_TEST_SUITE(Test_FooPtrVector_mod_suite)
ADD_TEST_SUITE(Test_FooPolyVector_mod_suite)
ADD_TEST_SUITE(Test_FooPolyPtrVector_mod_suite)

! Unlimited polymorphic
#ifndef __GFORTRAN__
ADD_TEST_SUITE(Test_UnlimitedPolyVector_mod_suite)
#endif
ADD_TEST_SUITE(Test_UnlimitedPolyPtrVector_mod_suite)


! Some compilers have bugs related to returning pointers to fixed
! length strings
#ifdef SUPPORT_FOR_POINTERS_TO_FIXED_LENGTH_STRINGS
  ADD_TEST_SUITE(Test_Character17Vector_mod_suite)
#endif

! And other compilers have bugs related to returning pointers to
! deferred length strings
#ifdef SUPPORT_FOR_POINTERS_TO_DEFERRED_LENGTH_STRINGS
  ADD_TEST_SUITE(Test_deferredLengthStringVector_mod_suite)
#endif


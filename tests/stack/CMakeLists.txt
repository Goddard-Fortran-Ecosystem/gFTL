set (types
  integer
  logical
  character_len17
  deferred_string
  unlimited
  integer_shape_3
  real_rank_2_deferred_shape
#  Foo
#  FooPoly
  )

set (src ${CMAKE_CURRENT_SOURCE_DIR})
set (bin ${CMAKE_CURRENT_BINARY_DIR})

foreach (T ${types})
  set (sut ${T}Stack.F90)
  set (test Test_${T}Stack)

  m4_preprocess (${T} ${src}/Stack.m4 ${sut})
  m4_preprocess (${T} ${src}/Test_Stack.m4 ${test}.pf)

  add_pfunit_ctest (${test}.x
    TEST_SOURCES ${bin}/${test}.pf
    OTHER_SOURCES ${sut}
    LINK_LIBRARIES test_shared gftl-v2
    )
  add_dependencies (tests ${test}.x)

endforeach ()


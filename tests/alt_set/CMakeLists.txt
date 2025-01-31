# Note that a set of elements of type LOGICAL is not
# supported.
set (types
  integer
  integer32
  character_len17
  deferred_string
  Foo
  )


set (src ${CMAKE_CURRENT_SOURCE_DIR})
set (bin ${CMAKE_CURRENT_BINARY_DIR})

foreach (T ${types})
  set (sut ${T}_alt_set.F90)
  set (test Test_${T}_alt_set)

  m4_preprocess (${T} ${src}/alt_set.m4 ${sut})
  m4_preprocess (${T} ${src}/Test_alt_set.m4 ${test}.pf)
  m4_preprocess (${T} ${src}/Test_alt_set_Algorithms.m4 ${test}Algorithms.pf)

  add_pfunit_ctest (${test}.x
    TEST_SOURCES ${bin}/${test}.pf ${bin}/${test}Algorithms.pf
    OTHER_SOURCES ${sut}
    LINK_LIBRARIES test_shared gftl-v2
    )

  add_dependencies (tests ${test}.x)

endforeach ()


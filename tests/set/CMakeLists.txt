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
  set (sut ${T}_Set.F90)
  set (test Test_${T}Set)

  m4_preprocess (${T} ${src}/Set.m4 ${sut})
  m4_preprocess (${T} ${src}/Test_Set.m4 ${test}.pf)
#  m4_preprocess (${T} ${src}/Test_SetIterator.m4 Test_${T}SetIterator.pf ${depends})
#  m4_preprocess (${T} ${src}/Test_SetRIterator.m4 Test_${T}SetRIterator.pf ${depends})
  m4_preprocess (${T} ${src}/Test_SetAlgorithms.m4 ${test}Algorithms.pf)

  add_pfunit_ctest (${test}.x
    TEST_SOURCES ${bin}/${test}.pf ${bin}/${test}Algorithms.pf #${bin}/${test}Iterator.pf
    OTHER_SOURCES ${sut}
    LINK_LIBRARIES test_shared gftl-v2
    )

  add_dependencies (tests ${test}.x)

endforeach ()


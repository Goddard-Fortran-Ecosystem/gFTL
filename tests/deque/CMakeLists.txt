set (types
  integer
  logical
  character_len17
  deferred_string
  unlimited
  integer_shape_3
  real_rank_2_deferred_shape
  Foo
  FooPoly
  )

set (src ${CMAKE_CURRENT_SOURCE_DIR})
set (bin ${CMAKE_CURRENT_BINARY_DIR})
foreach (T ${types})
  set (sut ${T}Deque.F90)
  set (test Test_${T}Deque)

  m4_preprocess (${T} ${src}/Deque.m4 ${sut})
  m4_preprocess (${T} ${src}/Test_Deque.m4 ${test}.pf)
  m4_preprocess (${T} ${src}/Test_DequeIterator.m4 ${test}Iterator.pf)
  m4_preprocess (${T} ${src}/Test_DequeRIterator.m4 ${test}RIterator.pf)
#  m4_preprocess (${T} ${src}/Test_DequeAlgorithms.m4 ${test}Algorithms.pf)

  add_pfunit_ctest (${test}.x
    TEST_SOURCES ${bin}/${test}.pf ${bin}/${test}Iterator.pf ${bin}/${test}RIterator.pf
    OTHER_SOURCES ${sut}
    LINK_LIBRARIES test_shared gftl-v2
    )

  add_dependencies (tests ${test}.x)

endforeach ()


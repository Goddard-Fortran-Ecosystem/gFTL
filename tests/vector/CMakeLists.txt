#add_subdirectory(sut)

find_program(M4 m4)

set (types
  integer
  logical
  character_len17
  deferred_string
  unlimited
  integer_shape_3
  real_rank_2_deferred_shape
  Foo
#  FooPoly
  )

set (src ${CMAKE_CURRENT_SOURCE_DIR})
set (bin ${CMAKE_CURRENT_BINARY_DIR})

foreach (T ${types})
  set (sut ${T}_Vector.F90)
  set (test Test_${T}Vector)

  # TODO - should be split into separate commands to streamline dependencies
  m4_preprocess (${T} ${src}/Vector.m4 ${sut})
  m4_preprocess (${T} ${src}/Test_Vector.m4 ${test}.pf)
  m4_preprocess (${T} ${src}/Test_VectorIterator.m4 ${test}Iterator.pf)
  m4_preprocess (${T} ${src}/Test_VectorRIterator.m4 ${test}RIterator.pf)
  m4_preprocess (${T} ${src}/Test_VectorAlgorithms.m4 ${test}Algorithms.pf)

  add_pfunit_ctest (${test}.x
    TEST_SOURCES ${bin}/${test}.pf
                 ${bin}/${test}Iterator.pf ${bin}/${test}RIterator.pf
                 ${bin}/${test}Algorithms.pf
    OTHER_SOURCES ${sut}
    LINK_LIBRARIES test_shared gftl-v2
    )

  add_dependencies (tests ${test}.x)

endforeach ()


# Note that a set of elements of type LOGICAL is not
# supported.
set (types
  integer
  character_len17
  deferred_string
  Foo
#  logical
#  unlimited
#  integer_shape_3
#  real_rank_2_deferred_shape
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

  add_pfunit_ctest (${test}.x
    TEST_SOURCES ${bin}/${test}.pf #${bin}/Test_${T}SetIterator.pf ${bin}/Test_${T}SetRIterator.pf
    OTHER_SOURCES ${sut}
    LINK_LIBRARIES test_shared gftl-v2
    )

  add_dependencies (tests ${test}.x)

endforeach ()


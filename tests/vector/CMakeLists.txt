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
#  Foo
#  FooPoly
  )

foreach (T ${types})
  set (sut ${T}Vector)
  set (test Test_${T}Vector)
  set (src ${CMAKE_CURRENT_SOURCE_DIR})
  set (bin ${CMAKE_CURRENT_BINARY_DIR})

  set (depends
    ${src}/Vector.m4
    ${src}/Test_Vector.m4
    ${src}/Test_VectorIterator.m4
    ${src}/Test_VectorAlgorithms.m4
    ${src}/Test_VectorRIterator.m4
    ${src}/${T}.inc
    ${src}/test_${T}.inc
    )

  set(WD ${CMAKE_CURRENT_BINARY_DIR}/${T})

  # TODO - should be split into separate commands to streamline dependencies
  add_custom_command (
    OUTPUT ${bin}/${sut}.F90 ${bin}/${test}.pf  ${bin}/${test}Iterator.pf ${bin}/${test}RIterator.pf
    COMMAND ${M4} -s -D_type=${T} ${src}/Vector.m4  > ${sut}.F90
    COMMAND ${M4} -s -D_type=${T} ${src}/Test_Vector.m4 > ${test}.pf
    COMMAND ${M4} -s -D_type=${T} ${src}/Test_VectorIterator.m4 > ${test}Iterator.pf
    COMMAND ${M4} -s -D_type=${T} ${src}/Test_VectorRIterator.m4 > ${test}RIterator.pf
    COMMAND ${M4} -s -D_type=${T} ${src}/Test_VectorAlgorithms.m4 > ${test}Algorithms.pf
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS ${depends}
    )

  set (other_sources ${bin}/${sut}.F90)
  if (T STREQUAL Foo OR T STREQUAL FooPoly)
    list (APPEND other_sources Foo.F90)
  endif ()

  add_pfunit_ctest (${test}.x
    TEST_SOURCES ${bin}/${test}.pf
                 ${bin}/${test}Iterator.pf ${bin}/${test}RIterator.pf
                 ${bin}/${test}Algorithms.pf
    OTHER_SOURCES ${other_sources}
    LINK_LIBRARIES gftl-v2
    )

  add_dependencies (tests ${test}.x)

endforeach ()


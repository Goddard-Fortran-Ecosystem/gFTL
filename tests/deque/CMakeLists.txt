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
  FooPoly
  )

foreach (T ${types})
  set (sut ${T}Deque)
  set (test Test_${T}Deque)
  set (src ${CMAKE_CURRENT_SOURCE_DIR})
  set (bin ${CMAKE_CURRENT_BINARY_DIR})

  set (depends
    ${src}/Deque.m4
    ${src}/Test_Deque.m4
    ${src}/Test_DequeIterator.m4
    ${src}/Test_DequeRIterator.m4
    ${src}/${T}.inc
    ${src}/test_${T}.inc
    )

  add_custom_command (
    OUTPUT ${bin}/${sut}.F90 ${bin}/${test}.pf  ${bin}/${test}Iterator.pf ${bin}/${test}RIterator.pf
    COMMAND ${M4} -s -D_type=${T} ${src}/Deque.m4  > ${sut}.F90
    COMMAND ${M4} -s -D_type=${T} ${src}/Test_Deque.m4 > ${test}.pf
    COMMAND ${M4} -s -D_type=${T} ${src}/Test_DequeIterator.m4 > ${test}Iterator.pf
    COMMAND ${M4} -s -D_type=${T} ${src}/Test_DequeRIterator.m4 > ${test}RIterator.pf
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS ${depends}
    )

  set (other_sources ${bin}/${sut}.F90)
  if (T STREQUAL Foo OR T STREQUAL FooPoly)
    list (APPEND other_sources Foo.F90)
  endif ()

  add_pfunit_ctest (${test}.x
    TEST_SOURCES ${bin}/${test}.pf ${bin}/${test}Iterator.pf ${bin}/${test}RIterator.pf
    OTHER_SOURCES ${other_sources}
    LINK_LIBRARIES gftl-v2
    )

  add_dependencies (tests ${test}.x)

endforeach ()


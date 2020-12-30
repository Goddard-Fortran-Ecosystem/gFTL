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
  set (sut ${T}Queue)
  set (test Test_${T}Queue)
  set (src ${CMAKE_CURRENT_SOURCE_DIR})
  set (bin ${CMAKE_CURRENT_BINARY_DIR})

  set (depends
    ${src}/Queue.m4
    ${src}/Test_Queue.m4
    ${src}/${T}.inc
    ${src}/test_${T}.inc
    )

  add_custom_command (
    OUTPUT ${bin}/${sut}.F90 ${bin}/${test}.pf
    COMMAND ${M4} -s -D_type=${T} ${src}/Queue.m4  > ${sut}.F90
    COMMAND ${M4} -s -D_type=${T} ${src}/Test_Queue.m4 > ${test}.pf
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS ${depends}
    )

  set (other_sources ${bin}/${sut}.F90)
  if (T STREQUAL Foo OR T STREQUAL FooPoly)
    list (APPEND other_sources Foo.F90)
  endif ()

  add_pfunit_ctest (${test}.x
    TEST_SOURCES ${bin}/${test}.pf
    OTHER_SOURCES ${other_sources}
    LINK_LIBRARIES gftl-v2
    )

  add_dependencies (tests ${test}.x)

endforeach ()


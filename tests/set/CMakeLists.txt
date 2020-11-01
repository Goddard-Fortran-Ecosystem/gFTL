find_program(M4 m4)

set (types
  integer
#  logical
#  character_len17
#  deferred_string
#  unlimited
#  integer_shape_3
#  real_rank_2_deferred_shape
#  Foo
  )


function (m4_preprocess T infile outfile depends)
  add_custom_command (
    OUTPUT ${outfile}
    COMMAND ${M4} -s -D_type=${T} ${infile} > ${outfile}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS ${infile} ${depends}
    )
endfunction()

foreach (T ${types})
  set (src ${CMAKE_CURRENT_SOURCE_DIR})
  set (bin ${CMAKE_CURRENT_BINARY_DIR})
  set (sut ${T}Set.F90)

  m4_preprocess (${T} ${src}/Set.m4 ${sut} "${T}.inc")
  set (depends "${src}/${T}.inc;${src}/test_${T}.inc")
  m4_preprocess (${T} ${src}/Test_Set.m4 Test_${T}Set.pf ${depends})
#  m4_preprocess (${T} ${src}/Test_SetIterator.m4 Test_${T}SetIterator.pf ${depends})
#  m4_preprocess (${T} ${src}/Test_SetRIterator.m4 Test_${T}SetRIterator.pf ${depends})

  set (other_sources ${sut})
#  if (T STREQUAL Foo)
#    list (APPEND other_sources Foo.F90)
#  endif ()

  add_pfunit_ctest (test_${T}_set.x
    TEST_SOURCES ${bin}/Test_${T}Set.pf #${bin}/Test_${T}SetIterator.pf ${bin}/Test_${T}SetRIterator.pf
    OTHER_SOURCES ${other_sources}
    LINK_LIBRARIES gftl-v2
    )

  add_dependencies (tests test_${T}_set.x)

endforeach ()


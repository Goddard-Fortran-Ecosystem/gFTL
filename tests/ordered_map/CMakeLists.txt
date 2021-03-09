find_program(M4 m4)

# Note that a set of elements of type LOGICAL is not
# supported.
set (keys
  integer
#  character_len17
#  deferred_string
#  Foo
#  logical
#  integer_shape_3
#  real_rank_2_deferred_shape
  )

set (types
  integer
#  character_len17
#  deferred_string
#  Foo
#  logical
#  unlimited
#  integer_shape_3
#  real_rank_2_deferred_shape
  )


function (m4_preprocess Key T infile outfile depends)
  add_custom_command (
    OUTPUT ${outfile}
    COMMAND ${M4} -s -D_key=${Key} -D_type=${T} ${infile} > ${outfile}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS ${infile} ${depends}
    )
endfunction()

foreach (key ${keys})
  foreach (T ${types})
    set (src ${CMAKE_CURRENT_SOURCE_DIR})
    set (bin ${CMAKE_CURRENT_BINARY_DIR})
    set (sut ${key}${T}OrderedMap.F90)

    m4_preprocess (${key} ${T} ${src}/OrderedMap.m4 ${sut} "${T}_T.inc")
  set (depends "${src}/${T}_T.inc;${src}/test_${T}_T.inc")
#    set (depends "${src}/${key}.inc;${src}/${T}.inc;${src}/test_${T}_T.inc;${src}/test_${key}_Key.inc")
    m4_preprocess (${key} ${T} ${src}/Test_OrderedMap.m4 Test_${key}${T}OrderedMap.pf ${depends})

    set (other_sources ${sut})
    if (T STREQUAL Foo OR key STREQUAL Foo)
      list (APPEND other_sources Foo.F90)
    endif ()

    add_pfunit_ctest (test_${key}${T}_ordered_map.x
      TEST_SOURCES ${bin}/Test_${key}${T}OrderedMap.pf
      OTHER_SOURCES ${other_sources}
      LINK_LIBRARIES gftl-v2
      )
    set_target_properties (test_${key}${T}_ordered_map.x PROPERTIES Fortran_MODULE_DIRECTORY test_${key}${T}_ordered_map/mod)
    add_dependencies (tests test_${key}${T}_ordered_map.x)

  endforeach ()
endforeach ()
  

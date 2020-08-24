add_subdirectory(SUT)


include_directories (${PFUNIT_TOP_DIR}/include)
link_directories(${PFUNIT_TOP_DIR}/lib)


set (src ${CMAKE_CURRENT_SOURCE_DIR})
set (bin ${CMAKE_CURRENT_BINARY_DIR})

set  (instantiations
# vary value type
  "integer\;integer"
  "integer\;real"
  "integer\;logical"
  "integer\;complex"
  "integer\;deferredLengthString"
  "integer\;Foo"
  "integer\;FooPoly"
  "integer\;integerAlloc"
  "integer\;integerPtr"
  "integer\;integer1d"
  "integer\;integer2d_fixedExtents"
  "integer\;integer2dPtr"

# vary key type
  "real32\;integer"
  "complex\;integer"
  "deferredLengthString\;integer"
  "Foo\;integer"
  "integerAlloc\;integer"
  "integerPtr\;integer"
  "integer1d\;integer"
  "integer2dPtr\;integer"

# This next one needs custom support in tests.
#  "deferredLengthString\;unlimitedPoly"

# duplicate combos
  "integerPtr\;integerPtr"
  "integer1d\;integer1d"
  "deferredLengthString\;deferredLengthString"
  )

set (infile ${src}/Test_Map.m4)

foreach (instantiation ${instantiations})
  list (GET instantiation 0 key)
  list (GET instantiation 1 value)

  set (pfunitfile Test_${key}${value}Map.pf)

  # Use relative path for outfile so that CMake correctly
  # detects the need to generate include files.

  add_custom_command (
    OUTPUT ${pfunitfile}
    COMMAND ${M4} -s -DKEY=${key} -DVALUE=${value} -DALT= -Dformat=${format} -I${GFTL_SOURCE_DIR}/include/templates < ${infile} > ${pfunitfile}
    WORKING_DIRECTORY ${bin}
    DEPENDS ${infile}
    )

  list (APPEND SRCS ${CMAKE_CURRENT_BINARY_DIR}/${pfunitfile} )


  set (pfunitfile Test_${key}${value}altMap.pf)

  # Use relative path for outfile so that CMake correctly
  # detects the need to generate include files.

  add_custom_command (
    OUTPUT ${pfunitfile}
    COMMAND ${M4} -s -DKEY=${key} -DVALUE=${value} -DALT=alt -Dformat=${format} -I${GFTL_SOURCE_DIR}/include/templates < ${infile} > ${pfunitfile}
    WORKING_DIRECTORY ${bin}
    DEPENDS ${infile}
    )

  list (APPEND SRCS ${CMAKE_CURRENT_BINARY_DIR}/${pfunitfile} )

endforeach ()


list (APPEND SRCS
  AuxTest.pf
  Test_map_Allocatable.pf
  Test_map_double_assign.pf)

add_pfunit_ctest (map_tests
  TEST_SOURCES ${SRCS}
  LINK_LIBRARIES mapSUT
  )

target_include_directories (map_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_include_directories (map_tests PRIVATE ${GFTL_SOURCE_DIR}/include)
add_dependencies (tests map_tests)


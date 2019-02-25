add_subdirectory(SUT)


include_directories (${PFUNIT}/include)
include_directories (${PFUNIT}/mod)
link_directories(${PFUNIT}/lib)


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
  set (outfile Test_${key}${value}Map.F90)

  # Use relative path for outfile so that CMake correctly
  # detects the need to generate include files.

  add_custom_command (
    OUTPUT ${outfile}
    COMMAND m4 -s -DKEY=${key} -DVALUE=${value} -DALT= -Dformat=${format} -I${GFTL_SOURCE_DIR}/include/templates < ${infile} > ${pfunitfile}
    COMMAND ${PFUNIT}/bin/pFUnitParser.py ${pfunitfile} ${outfile}
    WORKING_DIRECTORY ${bin}
    DEPENDS ${infile}
    )

  list (APPEND SRCS ${outfile} )


  set (pfunitfile Test_${key}${value}altMap.pf)
  set (outfile Test_${key}${value}altMap.F90)

  # Use relative path for outfile so that CMake correctly
  # detects the need to generate include files.

  add_custom_command (
    OUTPUT ${outfile}
    COMMAND m4 -s -DKEY=${key} -DVALUE=${value} -DALT=alt -Dformat=${format} -I${GFTL_SOURCE_DIR}/include/templates < ${infile} > ${pfunitfile}
    COMMAND ${PFUNIT}/bin/pFUnitParser.py ${pfunitfile} ${outfile}
    WORKING_DIRECTORY ${bin}
    DEPENDS ${infile}
    )

  list (APPEND SRCS ${outfile} )

endforeach ()


list (APPEND SRCS AuxTest.F90)

add_custom_command (
  OUTPUT Test_map_Allocatable.F90
  COMMAND ${PFUNIT}/bin/pFUnitParser.py ${src}/Test_map_Allocatable.pf Test_map_Allocatable.F90
  WORKING_DIRECTORY ${bin}
  DEPENDS ${src}/Test_map_Allocatable.pf
  )
list(APPEND SRCS Test_map_Allocatable.F90)

add_custom_command (
    OUTPUT AuxTest.F90
    COMMAND ${PFUNIT}/bin/pFUnitParser.py ${src}/AuxTest.pf AuxTest.F90
    WORKING_DIRECTORY ${bin}
    DEPENDS ${src}/AuxTest.pf
    )

add_library(testMap STATIC EXCLUDE_FROM_ALL ${SRCS})
target_include_directories (testMap PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_include_directories (testMap PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_link_libraries(testMap mapSUT pfunit)
add_dependencies(testMap type_test_values)

set(driver ${PFUNIT}/include/driver.F90)
add_executable(maptests.x EXCLUDE_FROM_ALL ${driver})
target_link_libraries(maptests.x testMap)

add_dependencies(tests maptests.x)
add_test(NAME maptests
  COMMAND maptests.x
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  )

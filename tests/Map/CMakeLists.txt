add_subdirectory(SUT)


include_directories ($ENV{PFUNIT}/include)
include_directories ($ENV{PFUNIT}/mod)
link_directories($ENV{PFUNIT}/lib)

include_directories (${CMAKE_CURRENT_SOURCE_DIR})
include_directories (${CMAKE_CURRENT_BINARY_DIR}/SUT)


include_directories (${FTL_BINARY_DIR}/mod)
include_directories(${FTL_BINARY_DIR}/tests/shared)
file(RELATIVE_PATH INCLUDE_RELATIVE_PATH_BIN
     ${CMAKE_CURRENT_SOURCE_DIR}
     ${CMAKE_BINARY_DIR}/.)
file(RELATIVE_PATH INCLUDE_RELATIVE_PATH_SRC
     ${CMAKE_CURRENT_SOURCE_DIR}
     ${CMAKE_SOURCE_DIR}/.)

include_directories (${INCLUDE_RELATIVE_PATH_BIN}/include)
include_directories (${INCLUDE_RELATIVE_PATH_SRC}/include)
include_directories(${INCLUDE_RELATIVE_PATH_BIN}/tests/include)
include_directories (${CMAKE_SOURCE_DIR}/include)
include_directories (${CMAKE_CURRENT_SOURCE_DIR}/include)



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
    COMMAND m4 -s -DKEY=${key} -DVALUE=${value} -DALT= -Dformat=${format} -I${CMAKE_SOURCE_DIR}/include/templates < ${infile} > ${pfunitfile}
    COMMAND $ENV{PFUNIT}/bin/pFUnitParser.py ${pfunitfile} ${outfile}
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
    COMMAND m4 -s -DKEY=${key} -DVALUE=${value} -DALT=alt -Dformat=${format} -I${CMAKE_SOURCE_DIR}/include/templates < ${infile} > ${pfunitfile}
    COMMAND $ENV{PFUNIT}/bin/pFUnitParser.py ${pfunitfile} ${outfile}
    WORKING_DIRECTORY ${bin}
    DEPENDS ${infile}
    )

  list (APPEND SRCS ${outfile} )

endforeach ()


list (APPEND SRCS AuxTest.F90)

add_custom_command (
    OUTPUT AuxTest.F90
    COMMAND $ENV{PFUNIT}/bin/pFUnitParser.py ${src}/AuxTest.pf AuxTest.F90
    WORKING_DIRECTORY ${bin}
    DEPENDS ${src}/AuxTest.pf
    )

add_library(testMap STATIC EXCLUDE_FROM_ALL ${SRCS})
target_link_libraries(testMap mapSUT pfunit)
add_dependencies(testMap types)
add_dependencies(testMap type_test_values)

set(driver $ENV{PFUNIT}/include/driver.F90)
add_executable(maptests.x EXCLUDE_FROM_ALL ${driver})
target_link_libraries(maptests.x pfunit testMap mapSUT shared ftl)
add_custom_target(maptests COMMAND maptests.x DEPENDS maptests.x)


add_subdirectory(SUT)


include_directories ($ENV{PFUNIT}/include)
include_directories ($ENV{PFUNIT}/mod)
link_directories($ENV{PFUNIT}/lib)

include_directories (${CMAKE_CURRENT_SOURCE_DIR})
include_directories (${FTL_BINARY_DIR}/mod)
include_directories (${FTL_BINARY_DIR}/tests/shared)

include_directories (${FTL_SOURCE_DIR}/include)
include_directories (${FTL_BINARY_DIR}/include)
include_directories (${CMAKE_CURRENT_SOURCE_DIR}/include)
include_directories (${CMAKE_CURRENT_BINARY_DIR}/SUT)



include_directories(${FTL_BINARY_DIR}/tests/shared)
# Use relative paths for includes so that CMake correctly
# detects the need to generate include files.
file(RELATIVE_PATH INCLUDE_RELATIVE_PATH_BIN
     ${CMAKE_CURRENT_SOURCE_DIR}
     ${CMAKE_BINARY_DIR}/.)
include_directories(${INCLUDE_RELATIVE_PATH_BIN}/tests/include)
include_directories(${INCLUDE_RELATIVE_PATH_BIN}/include)


set (src ${CMAKE_CURRENT_SOURCE_DIR})
set (bin ${CMAKE_CURRENT_BINARY_DIR})

set  (types
  integer
  real
  real64
  complex

  deferredLengthString
#  unlimitedPoly
  unlimitedPolyPtr

  integer1d
  integer2d
  integer2d_fixedExtents

  integerAlloc
  integerPtr
  integer2dPtr

  Foo
  FooPtr
  FooPoly
  FooPolyPtr
  )

if (SUPPORTS_POINTERS_TO_FIXED_LENGTH_STRINGS)
  LIST (APPEND types character17)
endif ()

set(SRCS)

foreach (type ${types} )
  set (infile ${src}/Test_Set.m4)
  set (pfunitfile Test_${type}Set.pf)
  set (outfile Test_${type}Set.F90)

  # Use relative path for outfile so that CMake correctly
  # detects the need to generate include files.

  add_custom_command (
    OUTPUT ${outfile}
    COMMAND m4 -s -Dparam=${type} -I${CMAKE_SOURCE_DIR}/include/templates < ${infile} > ${pfunitfile}
    COMMAND $ENV{PFUNIT}/bin/pFUnitParser.py ${pfunitfile} ${outfile}
    WORKING_DIRECTORY ${bin}
    DEPENDS ${infile}
    )

  list (APPEND SRCS ${outfile} )

endforeach ()



add_library (testSet STATIC EXCLUDE_FROM_ALL ${SRCS})
target_link_libraries (testSet shared setSUT pfunit)
add_dependencies (testSet types)
add_dependencies (testSet type_test_values)

set (driver $ENV{PFUNIT}/include/driver.F90)
add_executable (settests.x EXCLUDE_FROM_ALL ${driver})
target_link_libraries (settests.x  pfunit testSet setSUT shared ftl)
add_custom_target (settests COMMAND settests.x DEPENDS settests.x EXCLUDE_FROM_ALL)


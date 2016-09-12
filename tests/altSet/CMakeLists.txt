add_subdirectory(SUT)


include_directories (${PFUNIT}/include)
include_directories (${PFUNIT}/mod)
link_directories(${PFUNIT}/lib)

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
  set (infile ${src}/Test_altSet.m4)
  set (pfunitfile Test_${type}altSet.pf)
  set (outfile Test_${type}altSet.F90)

  # Use relative path for outfile so that CMake correctly
  # detects the need to generate include files.

  add_custom_command (
    OUTPUT ${outfile}
    COMMAND m4 -s -Dparam=${type} -I${CMAKE_SOURCE_DIR}/include/templates < ${infile} > ${pfunitfile}
    COMMAND ${PFUNIT}/bin/pFUnitParser.py ${pfunitfile} ${outfile}
    WORKING_DIRECTORY ${bin}
    DEPENDS ${infile}
    )

  list (APPEND SRCS ${outfile} )

endforeach ()



add_library (testaltSet STATIC EXCLUDE_FROM_ALL ${SRCS})
target_link_libraries (testaltSet shared altsetSUT pfunit)
add_dependencies (testaltSet types)
add_dependencies (testaltSet type_test_values)

set (driver ${PFUNIT}/include/driver.F90)
add_executable (setalttests.x EXCLUDE_FROM_ALL ${driver})
target_link_libraries (setalttests.x  pfunit testaltSet altsetSUT shared ftl)
add_custom_target (setalttests COMMAND setalttests.x DEPENDS setalttests.x EXCLUDE_FROM_ALL)


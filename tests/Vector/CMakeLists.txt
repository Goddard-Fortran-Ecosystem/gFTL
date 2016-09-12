add_subdirectory (SUT)


include_directories  ($ENV{PFUNIT}/include)
include_directories  ($ENV{PFUNIT}/mod)
link_directories ($ENV{PFUNIT}/lib)

include_directories  (${CMAKE_CURRENT_SOURCE_DIR})
include_directories  (${FTL_SOURCE_DIR}/include)
include_directories  (${CMAKE_CURRENT_SOURCE_DIR}/include)
include_directories  (${FTL_BINARY_DIR}/include)
include_directories  (${FTL_BINARY_DIR}/mod)
include_directories  (${CMAKE_CURRENT_BINARY_DIR}/SUT)
include_directories (${FTL_BINARY_DIR}/tests/shared)

file (RELATIVE_PATH INCLUDE_RELATIVE_PATH_BIN
     ${CMAKE_CURRENT_SOURCE_DIR}
     ${CMAKE_BINARY_DIR}/.)
include_directories (${INCLUDE_RELATIVE_PATH_BIN}/include)
include_directories (${INCLUDE_RELATIVE_PATH_BIN}/tests/include)


set (src ${CMAKE_CURRENT_SOURCE_DIR})
set (bin ${CMAKE_CURRENT_BINARY_DIR})

set  (types
  integer
  real
  real64
  complex
  logical
  unlimitedPoly
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

set (SRCS)
if (SUPPORT_FOR_POINTERS_TO_FIXED_LENGTH_STRINGS)
  LIST (APPEND types character17)
endif ()

if (SUPPORT_FOR_POINTERS_TO_DEFERRED_LENGTH_STRINGS)
  LIST (APPEND types deferredLengthString)
endif ()

foreach (type ${types} )
  set (infile ${src}/Test_Vector.m4)
  set (pfunitfile Test_${type}Vector.pf)
  set (outfile Test_${type}Vector.F90)

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

  set (infile ${src}/Test_VectorIterator.m4)
  set (pfunitfile Test_${type}VectorIterator.pf)
  set (outfile Test_${type}VectorIterator.F90)

  add_custom_command (
    OUTPUT ${outfile}
    COMMAND m4 -s -Dparam=${type} -I${CMAKE_SOURCE_DIR}/include/templates < ${infile} > ${pfunitfile}
    COMMAND $ENV{PFUNIT}/bin/pFUnitParser.py ${pfunitfile} ${outfile}
    WORKING_DIRECTORY ${bin}
    DEPENDS ${infile}
    )

  list (APPEND SRCS ${outfile} )

endforeach ()

add_library (testVector STATIC EXCLUDE_FROM_ALL ${SRCS})
target_link_libraries (testVector shared vectorSUT pfunit)
add_dependencies (testVector types)
add_dependencies (testVector type_test_values)

set (driver $ENV{PFUNIT}/include/driver.F90)
add_executable (vectortests.x EXCLUDE_FROM_ALL ${driver})
target_link_libraries (vectortests.x pfunit testVector vectorSUT shared ftl)
add_custom_target (vectortests COMMAND vectortests.x DEPENDS vectortests.x EXCLUDE_FROM_ALL)


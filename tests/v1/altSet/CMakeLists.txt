add_subdirectory(SUT)

set (src ${CMAKE_CURRENT_SOURCE_DIR})
set (bin ${CMAKE_CURRENT_BINARY_DIR})

set  (types
  integer
  real
  real64
  complex

  deferredLengthString
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

  # Use relative path for outfile so that CMake correctly
  # detects the need to generate include files.

  add_custom_command (
    OUTPUT ${pfunitfile}
    COMMAND ${M4} -s -Dparam=${type} -I${GFTL_SOURCE_DIR}/include/v1/templates < ${infile} > ${pfunitfile}
    WORKING_DIRECTORY ${bin}
    DEPENDS ${infile}
    )

  list (APPEND SRCS ${CMAKE_CURRENT_BINARY_DIR}/${pfunitfile} )

endforeach ()

add_pfunit_ctest (altSet_tests
  TEST_SOURCES ${SRCS}
  LINK_LIBRARIES altsetSUT
  )

target_include_directories (altSet_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/include)
add_dependencies (tests altSet_tests)

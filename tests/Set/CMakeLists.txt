add_subdirectory(SUT)


include_directories (${PFUNIT}/include)
include_directories (${PFUNIT}/mod)
link_directories(${PFUNIT}/lib)


set (src ${CMAKE_CURRENT_SOURCE_DIR})
set (bin ${CMAKE_CURRENT_BINARY_DIR})

set  (types
  integer
  real
  real64
  complex

  deferredLengthString

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

  unlimitedPolyPtr

# Unsupportable combinations:
# unlimitedPoly
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
    COMMAND m4 -s -Dparam=${type} -I${GFTL_SOURCE_DIR}/include/templates < ${infile} > ${pfunitfile}
    COMMAND ${PFUNIT}/bin/pFUnitParser.py ${pfunitfile} ${outfile}
    WORKING_DIRECTORY ${bin}
    DEPENDS ${infile}
    )

  list (APPEND SRCS ${outfile} )

endforeach ()



add_library (testSet STATIC EXCLUDE_FROM_ALL ${SRCS})
target_include_directories (testSet PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_include_directories (testSet PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_link_libraries (testSet setSUT pfunit)

set (driver ${PFUNIT}/include/driver.F90)
add_executable (settests.x EXCLUDE_FROM_ALL ${driver})
target_link_libraries (settests.x  pfunit testSet)
add_custom_target (settests COMMAND settests.x DEPENDS settests.x EXCLUDE_FROM_ALL)


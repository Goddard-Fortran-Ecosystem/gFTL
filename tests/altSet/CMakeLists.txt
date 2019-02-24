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
    COMMAND m4 -s -Dparam=${type} -I${GFTL_SOURCE_DIR}/include/templates < ${infile} > ${pfunitfile}
    COMMAND ${PFUNIT}/bin/pFUnitParser.py ${pfunitfile} ${outfile}
    WORKING_DIRECTORY ${bin}
    DEPENDS ${infile}
    )

  list (APPEND SRCS ${outfile} )

endforeach ()

add_custom_command (
  OUTPUT Test_altSet_Allocatable.F90
  COMMAND ${PFUNIT}/bin/pFUnitParser.py ${src}/Test_altSet_Allocatable.pf Test_altSet_Allocatable.F90
  WORKING_DIRECTORY ${bin}
  DEPENDS ${src}/Test_altSet_Allocatable.pf
  )
list(APPEND SRCS Test_altSet_Allocatable.F90)


add_library (testaltSet STATIC EXCLUDE_FROM_ALL ${SRCS})
target_include_directories (testaltSet PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_include_directories (testaltSet PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_link_libraries (testaltSet altsetSUT pfunit)
add_dependencies (testaltSet type_test_values)


set (driver ${PFUNIT}/include/driver.F90)
add_executable (setalttests.x EXCLUDE_FROM_ALL ${driver})
target_link_libraries (setalttests.x  testaltSet pfunit)
add_custom_target (setalttests COMMAND setalttests.x DEPENDS setalttests.x EXCLUDE_FROM_ALL)


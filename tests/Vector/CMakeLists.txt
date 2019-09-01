add_subdirectory (SUT)

include_directories  (${PFUNIT_TOP_DIR}/include)
link_directories (${PFUNIT_TOP_DIR}/lib)

set (src ${CMAKE_CURRENT_SOURCE_DIR})
set (bin ${CMAKE_CURRENT_BINARY_DIR})
message( "Vecttor Build type?: ${CMAKE_BUILD_TYPE}")

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

set (SRCS Test_nested.pf)
list(APPEND SRCS Test_vector_Allocatable.pf)

if (SUPPORT_FOR_POINTERS_TO_FIXED_LENGTH_STRINGS)
  LIST (APPEND types character17)
endif ()

if (SUPPORT_FOR_POINTERS_TO_DEFERRED_LENGTH_STRINGS)
  LIST (APPEND types deferredLengthString)
endif ()

foreach (type ${types} )
  set (infile ${src}/Test_Vector.m4)
  set (pfunitfile Test_${type}Vector.pf)

  # Use relative path for outfile so that CMake correctly
  # detects the need to generate include files.

  add_custom_command (
    OUTPUT ${pfunitfile}
    COMMAND m4 -s -Dparam=${type} -I${GFTL_SOURCE_DIR}/include/templates < ${infile} > ${pfunitfile}
    WORKING_DIRECTORY ${bin}
    DEPENDS ${infile}
    )

  list (APPEND SRCS ${CMAKE_CURRENT_BINARY_DIR}/${pfunitfile} )

  set (infile ${src}/Test_VectorIterator.m4)
  set (pfunitfile Test_${type}VectorIterator.pf)

  add_custom_command (
    OUTPUT ${pfunitfile}
    COMMAND m4 -s -Dparam=${type} -I${GFTL_SOURCE_DIR}/include/templates < ${infile} > ${pfunitfile}
    WORKING_DIRECTORY ${bin}
    DEPENDS ${infile}
    )

  list (APPEND SRCS ${CMAKE_CURRENT_BINARY_DIR}/${pfunitfile} )

endforeach ()


if(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 19)
    if(CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER 17)
      set_property(SOURCE Test_nested.F90 PROPERTY COMPILE_DEFINITIONS "__ifort_18")
    endif()
  endif()
endif()


add_pfunit_ctest (vectortests
  TEST_SOURCES ${SRCS}
  LINK_LIBRARIES vectorSUT
  )

target_include_directories(vectortests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_include_directories(vectortests PRIVATE ${GFTL_SOURCE_DIR}/include)



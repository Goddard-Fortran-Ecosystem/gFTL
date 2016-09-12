include_directories (${FTL_SOURCE_DIR}/include)
include_directories (${FTL_BINARY_DIR}/include)
include_directories (${PFUNIT}/mod)
include_directories (${FTL_BINARY_DIR}/mod)
include_directories(${FTL_BINARY_DIR}/tests/shared)
file(RELATIVE_PATH INCLUDE_RELATIVE_PATH_BIN
     ${CMAKE_CURRENT_SOURCE_DIR}
     ${CMAKE_BINARY_DIR}/.)
include_directories(${INCLUDE_RELATIVE_PATH_BIN}/include)

set(src ${CMAKE_CURRENT_SOURCE_DIR})
set(bin ${CMAKE_CURRENT_BINARY_DIR})


set (instantiations
  "integer\;free"
  "real\;free"
  "real32\;free"
  "real64\;free"
  "complex\;free"
  "complex64\;free"
  "complex128\;free"
  "logical\;free"
  "deferredLengthString\;free"
  "unlimitedPoly\;free"
  "unlimitedPolyPtr\;free"

  "integer1d\;free"
  "integer2d\;free"
  "integer2d_fixedExtents\;free"

  "integerAlloc\;free"
  "integerPtr\;free"
  "integer2dPtr\;free"

  "integer\;fixed"
 
  "Foo\;free"
  "FooPtr\;free"
  "FooPoly\;free"
  "FooPolyPtr\;free"
    
  )

if (SUPPORT_FOR_POINTERS_TO_FIXED_LENGTH_STRINGS)
  LIST (APPEND instantiations "character17\;free")
  message("***************")
endif ()

set (SRCS)
set (infile ${src}/Vector.m4)
# Empty list - will append in loop below
foreach (instantiation ${instantiations})
  list (GET instantiation 0 type)
  list (GET instantiation 1 format)

  if (format STREQUAL "free")
     set (outfile ${type}Vector.F90)
  else ()
     set (outfile ${type}Vector.F)
  endif ()

  # Use relative path for outfile so that CMake correctly
  # detects the need to generate include files.

  add_custom_command (
    OUTPUT ${outfile}
    COMMAND m4 -s -Dtype=${type} -Dformat=${format} < ${infile} > ${outfile}
    WORKING_DIRECTORY ${bin}
    DEPENDS ${infile}
    )
  
  list (APPEND SRCS ${outfile} )

endforeach ()

list (APPEND SRCS MultiModule.F90)

add_library(vectorSUT EXCLUDE_FROM_ALL STATIC ${SRCS})
add_dependencies(vectorSUT shared template_macros types)

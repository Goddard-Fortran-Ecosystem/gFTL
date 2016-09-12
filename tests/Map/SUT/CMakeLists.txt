set(src ${CMAKE_CURRENT_SOURCE_DIR})
set(bin ${CMAKE_CURRENT_BINARY_DIR})

set (instantiations
# vary value type
  "integer\;integer\;free"
  "integer\;real\;free"
  "integer\;logical\;free"
  "integer\;complex\;free"
  "integer\;deferredLengthString\;free"
  "integer\;Foo\;free"
  "integer\;FooPoly\;free"
  "integer\;integerAlloc\;free"
  "integer\;integerPtr\;free"
  "integer\;integer1d\;free"
  "integer\;integer2d_fixedExtents\;free"
  "integer\;integer2dPtr\;free"

# vary key type
  "real32\;integer\;free"
  "complex\;integer\;free"
  "deferredLengthString\;integer\;free"
  "Foo\;integer\;free"
  "integerAlloc\;integer\;free"
  "integerPtr\;integer\;free"
  "integer1d\;integer\;free"
  "integer2dPtr\;integer\;free"

  "deferredLengthString\;unlimitedPoly\;free"

# duplication combos
  "integerPtr\;integerPtr\;free"
  "integer1d\;integer1d\;free"
  "deferredLengthString\;deferredLengthString\;free"
)

include_directories (${FTL_BINARY_DIR}/mod)
include_directories(${FTL_BINARY_DIR}/tests/shared)
include_directories (${FTL_SOURCE_DIR}/include)
include_directories (${FTL_BINARY_DIR}/include)
include_directories ($ENV{PFUNIT}/mod)


set (SRCS)
set (altSRCS)
set (infile ${src}/Map.m4)
# Empty list - will append in loop below
foreach (instantiation ${instantiations})
  list (GET instantiation 0 key)
  list (GET instantiation 1 value)
  list (GET instantiation 2 format)

  if (format STREQUAL "free")
     set (outfile ${key}${value}Map.F90)
  else ()
     set (outfile ${key}${value}Map.F)
  endif ()

  # Use relative path for outfile so that CMake correctly
  # detects the need to generate include files.
  
  add_custom_command (
    OUTPUT ${outfile}
    COMMAND m4 -s -Dkey=${key} -DALT -Dvalue=${value} -Dformat=${format} < ${infile} > ${outfile}
    WORKING_DIRECTORY ${bin}
    DEPENDS ${infile}
    )

list (APPEND SRCS ${outfile} )

  if (format STREQUAL "free")
     set (outfile ${key}${value}altMap.F90)
  else ()
     set (outfile ${key}${value}altMap.F)
  endif ()

  add_custom_command (
    OUTPUT ${outfile}
    COMMAND m4 -s -Dkey=${key} -DALT=alt -Dvalue=${value} -Dformat=${format} < ${infile} > ${outfile}
    WORKING_DIRECTORY ${bin}
    DEPENDS ${infile}
    )

list (APPEND altSRCS ${outfile} )

endforeach ()

list (APPEND SRCS MultiModule.F90)

add_library(mapSUT STATIC EXCLUDE_FROM_ALL ${SRCS} ${altSRCS})
add_dependencies(mapSUT shared template_macros ftl types)

set(src ${CMAKE_CURRENT_SOURCE_DIR})
set(bin ${CMAKE_CURRENT_BINARY_DIR})

set (template_params type key value)
set (macro_files
  integer
  real
  real32
  real64
  complex
  complex64
  complex128
  logical
  character
  deferredLengthString
  character17
  unlimitedPoly
  unlimitedPolyPtr
  
  integer1d
  integer2d
  integer2d_fixedExtents

  integerPtr
  integer2dPtr
  integerAlloc

  Foo
  FooPtr
  FooPoly
  FooPolyPtr
  )

# Empty list - will append in loop below
set (generated_incs)

foreach (macro_file ${macro_files})
  foreach (param ${template_params})
    set (infile ${src}/${macro_file}.m4)
    if (${param} STREQUAL type)
      set (outfile ${macro_file}.inc)
    else ()
      set (outfile ${param}_${macro_file}.inc)
    endif ()
    set (outpath ${CMAKE_CURRENT_BINARY_DIR}/${outfile})

    add_custom_command (
      OUTPUT ${outfile}
      COMMAND ${M4} -s -Dparam=${param} -I${src}/../templates < ${infile} > ${outfile}
      WORKING_DIRECTORY ${bin}
      DEPENDS ${infile}
      )

    list (APPEND generated_incs ${outfile} )

  endforeach ()
endforeach ()

add_custom_target (generate-type-incs
  DEPENDS ${generated_incs}
  )
add_dependencies (gftl generate-type-incs)

set_source_files_properties (${generated_incs} PROPERTIES GENERATED TRUE)

install (DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/ DESTINATION "${dest}/include/types"
  FILES_MATCHING PATTERN "*.inc"
  PATTERN CMakeFiles EXCLUDE
  PATTERN "*Foo*" EXCLUDE
  )


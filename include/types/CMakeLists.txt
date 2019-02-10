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
      OUTPUT ${outpath}
      COMMAND m4 -s -Dparam=${param} -I${src}/../templates < ${infile} > ${outfile}
      WORKING_DIRECTORY ${bin}
      DEPENDS ${infile}
      )

    add_custom_target(
      generate-${outfile}
      DEPENDS ${outpath}
      )

    add_dependencies (gFTL generate-${outfile})

#    target_sources (types INTERFACE
#      $<BUILD_INTERFACE:${outfile}>
#      $<INSTALL_INTERFACE:${outfile}> # for client in install mode
#      )
    list (APPEND generated_incs ${outfile} )

  endforeach ()
endforeach ()

set_source_files_properties (${generated_incs} PROPERTIES GENERATED TRUE)

set (include_dest "include/gFTL-${gFTL_VERSION_MAJOR}.${gFTL_VERSION_MINOR}")

#target_include_directories (types INTERFACE
#  $<BUILD_INTERFACE:${gFTL_BINARY_DIR}/include> # for headers when building
#  $<INSTALL_INTERFACE:${include_dest}> # for client in install mode
#)

install (DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/ DESTINATION "${include_dest}/types"
  FILES_MATCHING PATTERN "*.inc"
  PATTERN CMakeFiles EXCLUDE
  )

#install (TARGETS types EXPORT types DESTINATION "${include_dest}/types")
#install (EXPORT types DESTINATION "${include_dest}")

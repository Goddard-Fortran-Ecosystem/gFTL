set(src ${CMAKE_CURRENT_SOURCE_DIR})
set(bin ${CMAKE_CURRENT_BINARY_DIR})

# Need to create a 2D array of include files.
# First axis is {type,key,value}
set (template_params type key value)

# Second axis is role
set (macro_files
  template_macros
  template_macros_undefs
  testing_macros
  set_use_tokens
  use_tokens_undef
  macros_undefs)

# Empty list - will append in loop below
set(generated_incs)

foreach( macro_file ${macro_files} )
  foreach( param ${template_params} )

    set( infile ${src}/${macro_file}.m4 )
    set( outfile ${param}_${macro_file}.inc )
    set (outpath ${CMAKE_CURRENT_BINARY_DIR}/${outfile})

    add_custom_command (
      OUTPUT ${outfile}
      COMMAND ${M4} -s -Dparam=${param} -I${src}/../templates < ${infile} > ${outfile}
      WORKING_DIRECTORY ${bin}
      DEPENDS ${infile}
      )

      list (APPEND generated_incs ${outfile} )

  endforeach()
endforeach()

add_custom_target(
  generate-template-incs
  DEPENDS ${generated_incs}
  )

add_dependencies (gftl generate-template-incs)

file (COPY DIRECTORY . DESTINATION . FILES_MATCHING PATTERN "*.inc")

install (DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/ DESTINATION "${dest}/include/templates"
  FILES_MATCHING PATTERN "*.inc"
  PATTERN CMakeFiles EXCLUDE
  )



# This directory uses m4 to generate various CPP/FPP include files that
# are used by gFTL templates.  

add_subdirectory(tests)

find_program( M4 m4)
if( NOT M4 )
  message( SEND_ERROR "m4 program not found" )
endif()

set (m4_sources
  define_derived_macros.m4
  undef_derived_macros.m4
  )

# Start with an empty list and append as we loop over cases.
set (generated_incs)

foreach (input_file ${m4_sources})
  get_filename_component(base_name ${input_file} NAME_WLE)

  foreach (parameter ${template_parameters})
    set (output_file ${base_name}_for_${parameter}.inc)

    add_custom_command (
      OUTPUT ${output_file}
      COMMAND ${M4} -s -D_T=${parameter} -I. < ${CMAKE_CURRENT_SOURCE_DIR}/${input_file} > ${output_file}
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
      DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${input_file}
      )

    list (APPEND generated_incs ${output_file})

  endforeach()
endforeach()

add_custom_target(m4_generated_includes ALL DEPENDS ${generated_incs})
add_dependencies(gftl m4_generated_includes)

install (DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/ DESTINATION "${dest}/include/generated"
  FILES_MATCHING PATTERN "*.inc"
  PATTERN CMakeFiles EXCLUDE
  )

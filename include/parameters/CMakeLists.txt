# This directory uses m4 to generate various CPP/FPP include files that
# are used by gFTL templates.  

find_program( M4 m4)
if( NOT M4 )
  message( SEND_ERROR "m4 program not found" )
endif()

function (process_m4 parameter input_file output_file)
  message("processing: ${input_file} and ${output_file}")
  add_custom_command (
    OUTPUT ${output_file}
    COMMAND ${M4} -s -D_T=${parameter} -I. ${CMAKE_CURRENT_SOURCE_DIR}/${input_file} > ${output_file}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${input_file}
    )
  list (APPEND type_incs ${output_file})
  set (type_incs ${type_incs} PARENT_SCOPE)

endfunction ()
  

# Start with an empty list and append as we loop over cases.

set (type_incs)
foreach (parameter ${template_parameters})
  file (MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${parameter})

  process_m4 (${parameter} define_internal.m4 ${parameter}/define_internal.inc)
  process_m4 (${parameter} undef_internal.m4 ${parameter}/undef_internal.inc)

  process_m4 (${parameter} define_derived_macros.m4 ${parameter}/define_derived_macros.inc)
  process_m4 (${parameter} undef_derived_macros.m4 ${parameter}/undef_derived_macros.inc)

endforeach()

add_subdirectory(tests)


add_custom_target(m4_type_includes ALL DEPENDS ${type_incs})
add_dependencies(gftl m4_type_includes)

foreach (parameter ${template_parameters})
  install (DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${parameter} DESTINATION "${dest}/include/parameters"
    FILES_MATCHING PATTERN "*.inc"
    PATTERN CMakeFiles EXCLUDE
    )
endforeach()

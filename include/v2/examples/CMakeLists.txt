find_program( M4 m4)
if( NOT M4 )
  message( SEND_ERROR "m4 program not found" )
endif()

set (base_types

  character
  deferred_length_string

  logical

  integer
  integer8
  integer16
  integer32
  integer64

  real
  real16
  real32
  real64
  real128
  double_precision

  complex
  complex16
  complex32
  complex64
  complex128
  )

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
  
set (type_incs)
foreach (parameter ${template_parameters})
  file (MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${parameter})

  foreach (type ${base_types} ${demo_types})
    get_filename_component(base_name ${type} NAME_WLE)

    process_m4 (${parameter} ${base_name}.m4 ${parameter}/${base_name}.inc)
    process_m4 (${parameter} undef_${base_name}.m4 ${parameter}/undef_${base_name}.inc)

  endforeach()

endforeach()

install (DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/ DESTINATION "${dest}/include/examples"
  FILES_MATCHING PATTERN "*.inc"
  PATTERN CMakeFiles EXCLUDE
  )

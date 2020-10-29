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
  
function (cpp_copy_parameter T_in T_out output_file)
  add_custom_command (
    OUTPUT ${output_file}
    COMMAND ${M4} -s -D_T_in=${T_in} -D_T_out=${T_out} -I. ${CMAKE_CURRENT_SOURCE_DIR}/cpp_copy.m4 > ${output_file}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/cpp_copy.m4
    )
  list (APPEND type_incs ${output_file})
  set (type_incs ${type_incs} PARENT_SCOPE)

endfunction ()
  
# Start with an empty list and append as we loop over cases.

set (type_incs)
foreach (parameter ${template_parameters})
  file (MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${parameter})

  process_m4 (${parameter} cpp_undef.m4 ${parameter}/undef_internal.inc)
  process_m4 (${parameter} define_derived_macros.m4 ${parameter}/define_derived_macros.inc)
  process_m4 (${parameter} undef_derived_macros.m4 ${parameter}/undef_derived_macros.inc)

endforeach ()

# Template specific extras

#pair
cpp_copy_parameter (T1 __pair_T1  T1/copy_T1_to_pair_T1.inc)
cpp_copy_parameter (T2 __pair_T2  T2/copy_T2_to_pair_T2.inc)
cpp_copy_parameter (__pair_T1 __T1 T1/copy_pair_T1_to_internal_T1.inc)
cpp_copy_parameter (__pair_T2 __T2 T2/copy_pair_T2_to_internal_T2.inc)
process_m4 (pair_T1 cpp_undef.m4 T1/undef_pair_T1.inc)
process_m4 (pair_T2 cpp_undef.m4 T2/undef_pair_T2.inc)

#ptr
cpp_copy_parameter (T __ptr_T  T/copy_T_to_ptr_T.inc)
cpp_copy_parameter (__ptr_T __T T/copy_ptr_T_to_internal_T.inc)
process_m4 (ptr_T cpp_undef.m4 T/undef_ptr_T.inc)

# vector
cpp_copy_parameter (T __vector_T T/copy_T_to_vector_T.inc)
cpp_copy_parameter (__vector_T __T T/copy_vector_T_to_internal_T.inc)
process_m4 (vector_T cpp_undef.m4 T/undef_vector_T.inc)

#set
cpp_copy_parameter (T __set_T T/copy_T_to_set_T.inc)
cpp_copy_parameter (__set_T __T T/copy_set_T_to_internal_T.inc)
process_m4 (set_T cpp_undef.m4 T/undef_set_T.inc)

# altset
cpp_copy_parameter (__set_T __vector_T T/copy_set_T_to_vector_T.inc)

# map
cpp_copy_parameter (Key __map_Key Key/copy_Key_to_map_Key.inc)
cpp_copy_parameter (__map_Key __pair_T1 T1/copy_map_Key_to_pair_T1.inc)
cpp_copy_parameter (__map_Key __Key Key/copy_map_Key_to_internal_Key.inc)

cpp_copy_parameter (T __map_T T/copy_T_to_map_T.inc)
cpp_copy_parameter (__map_T __pair_T2 T2/copy_map_T_to_pair_T2.inc)
cpp_copy_parameter (__map_T __T T/copy_map_T_to_internal_T.inc)
process_m4 (map_Key cpp_undef.m4 Key/undef_map_Key.inc)
process_m4 (map_T cpp_undef.m4 T/undef_map_T.inc)


add_subdirectory(tests)


add_custom_target(m4_type_includes ALL DEPENDS ${type_incs})
add_dependencies(gftl-v2 m4_type_includes)

foreach (parameter ${template_parameters})
  install (DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${parameter} DESTINATION "${dest}/include/parameters"
    FILES_MATCHING PATTERN "*.inc"
    PATTERN CMakeFiles EXCLUDE
    )
endforeach()

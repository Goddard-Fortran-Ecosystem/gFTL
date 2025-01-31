find_program(M4 m4)

function (m4_preprocess T infile outfile)
  add_custom_command (
    OUTPUT ${outfile}
    COMMAND ${M4} -s -D_type=${T} ${infile} > ${outfile}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS ${infile}
    )
endfunction()

function (m4_preprocess2 Key T infile outfile)
  add_custom_command (
    OUTPUT ${outfile}
    COMMAND ${M4} -s -D_key=${Key} -D_type=${T} ${infile} > ${outfile}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS ${infile}
    )
endfunction()

add_subdirectory(shared)
add_subdirectory(vector)
add_subdirectory(deque)
add_subdirectory(queue)
add_subdirectory(stack)
add_subdirectory(set)
add_subdirectory(alt_set)
add_subdirectory(map)
add_subdirectory(ordered_map)

# tests for legacy version 1
add_subdirectory(v1)

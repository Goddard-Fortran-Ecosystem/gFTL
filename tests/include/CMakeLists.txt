add_library (type_test_values INTERFACE)
target_include_directories (type_test_values INTERFACE ${CMAKE_CURRENT_BINARY_DIR})

add_subdirectory (types)
add_subdirectory (type_test_values) 

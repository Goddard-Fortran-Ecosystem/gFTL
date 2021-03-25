add_library (test_shared Foo.F90)
target_link_libraries(test_shared PRIVATE PFUNIT::funit)
target_include_directories(test_shared INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/include ${CMAKE_CURRENT_BINARY_DIR})

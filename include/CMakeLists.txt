
add_library (gftl INTERFACE)
target_include_directories (gftl INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}> # for headers when building
  $<INSTALL_INTERFACE:${dest}/include>           # for client in install mode
  )


#add_subdirectory(shared)
add_subdirectory(generated)
#add_subdirectory(templates)


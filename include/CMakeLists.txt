set (dest "gFTL-${gFTL_VERSION}")

add_library (gftl INTERFACE)
target_include_directories (gftl INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}> # for headers when building
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
  $<INSTALL_INTERFACE:${dest}/include>           # for client in install mode
  )

# Union of parameters across all supported
# templates.
set (template_parameters
  T # vector, set, map
  Key # map
  T1 # pair
  T2 # pair
  )

add_subdirectory(shared)
add_subdirectory(parameters)
add_subdirectory(templates)


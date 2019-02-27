add_library (gftl INTERFACE)
set (dest "GFTL-${GFTL_VERSION_MAJOR}.${GFTL_VERSION_MINOR}")
target_include_directories (gftl INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}> # for headers when building
  $<INSTALL_INTERFACE:${dest}/include> # for client in install mode
)

add_subdirectory (templates)
add_subdirectory (types)


add_custom_command(
  OUTPUT GFTL_incs_generated_successfully
  DEPENDS gftl
  COMMAND touch fidl_generated_successfully
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  )

add_custom_target (force-generation-of-includes ALL)
add_dependencies (force-generation-of-includes gftl)

install (TARGETS gftl EXPORT GFTL DESTINATION "${dest}/include")
install (EXPORT GFTL DESTINATION "${dest}/cmake")



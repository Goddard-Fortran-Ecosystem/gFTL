add_library (gFTL INTERFACE)
set (include_dest "include/gFTL-${gFTL_VERSION_MAJOR}.${gFTL_VERSION_MINOR}")
target_include_directories (gFTL INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}> # for headers when building
  $<INSTALL_INTERFACE:${include_dest}> # for client in install mode
)

add_subdirectory (templates)
add_subdirectory (types)

add_custom_target (force-generation-of-includes ALL)
add_dependencies (force-generation-of-includes gFTL)

install (TARGETS gFTL EXPORT gFTL DESTINATION "${include_dest}")
install (EXPORT gFTL DESTINATION "${include_dest}")



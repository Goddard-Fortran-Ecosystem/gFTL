add_library (gftl-v1 INTERFACE)
add_library (gftl INTERFACE)
target_link_libraries(gftl INTERFACE gftl-v1)
add_library (GFTL::gftl-v1 ALIAS gftl-v1)
add_library (GFTL::gftl ALIAS gftl)


set (dest "GFTL-${GFTL_VERSION_MAJOR}.${GFTL_VERSION_MINOR}")

target_include_directories (gftl-v1 INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}> # for headers when building
  $<INSTALL_INTERFACE:${dest}/include/v1>           # for client in install mode
  )

find_program (M4 m4)
if( NOT M4 )
  message( SEND_ERROR "m4 program not found" )
endif()

add_subdirectory (templates)
add_subdirectory (types)

add_custom_target (force-generation-of-includes ALL)
add_dependencies (force-generation-of-includes gftl-v1)

install (TARGETS gftl-v1 gftl EXPORT GFTL DESTINATION "${dest}/include/v1")



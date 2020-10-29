set (dest "gFTL-${gFTL_VERSION}")

add_library (gftl INTERFACE)
target_include_directories (gftl INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}> # for headers when building
  $<INSTALL_INTERFACE:${dest}/include>           # for client in install mode
  )

find_program (M4 m4)
if( NOT M4 )
  message( SEND_ERROR "m4 program not found" )
endif()

add_subdirectory (templates)
add_subdirectory (types)

add_custom_target (force-generation-of-includes ALL)
add_dependencies (force-generation-of-includes gftl)

install (TARGETS gftl EXPORT gFTL DESTINATION "${dest}/include")
install (EXPORT gFTL NAMESPACE gFTL:: DESTINATION "${dest}/lib/cmake/gFTL")



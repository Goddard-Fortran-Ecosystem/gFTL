add_library (gftl-v2 INTERFACE)
add_library (GFTL::gftl-v2 ALIAS gftl-v2)

set (dest "GFTL-${GFTL_VERSION_MAJOR}.${GFTL_VERSION_MINOR}")

target_include_directories (gftl-v2 INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}> # for headers when building
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
  $<INSTALL_INTERFACE:${dest}/include/v2>           # for client in install mode
  )

# Union of parameters across all supported
# templates.
set (template_parameters
  T # vector, deque, queue, stack, set, map, ordered_map
  Key # map, ordered_map
  T1 # pair
  T2 # pair
  )

find_program( M4 m4)
if( NOT M4 )
  message( SEND_ERROR "m4 program not found" )
endif()

add_subdirectory(shared)
add_subdirectory(parameters)

foreach (template ptr pair vector deque queue stack set alt_set map ordered_map algorithms)
  install (DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${template} DESTINATION "${dest}/include/v2"
    PATTERN CMakeLists.txt EXCLUDE
    PATTERN *~ EXCLUDE
    )
endforeach ()

install (TARGETS gftl-v2 EXPORT GFTL DESTINATION "${dest}/include/v2")

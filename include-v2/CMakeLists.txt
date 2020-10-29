set (dest "gFTL-${gFTL_VERSION}")

add_library (gftl-v2 INTERFACE)
target_include_directories (gftl-v2 INTERFACE
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

find_program( M4 m4)
if( NOT M4 )
  message( SEND_ERROR "m4 program not found" )
endif()

add_subdirectory(shared)
add_subdirectory(parameters)

foreach (template ptr pair vector set altset map)
  install (DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/ DESTINATION "${dest}/include"
    PATTERN CMakeLists.txt EXCLUDE
    PATTERN *~ EXCLUDE
    )
endforeach ()


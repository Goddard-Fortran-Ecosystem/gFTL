find_program( M4 m4)
if( NOT M4 )
  message( SEND_ERROR "m4 program not found" )
endif()

install (DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/ DESTINATION "${dest}/include/templates"
    PATTERN CMakeLists.txt EXCLUDE
    PATTERN *~ EXCLUDE
    )

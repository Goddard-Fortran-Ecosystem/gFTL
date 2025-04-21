install (DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/ DESTINATION "${dest}/include/templates"
    PATTERN CMakeLists.txt EXCLUDE
    PATTERN *~ EXCLUDE
    )

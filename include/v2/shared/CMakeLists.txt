install (DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/ DESTINATION "${dest}/include/v2/shared"
  PATTERN *.inc
  PATTERN CMakeLists.txt EXCLUDE
  )

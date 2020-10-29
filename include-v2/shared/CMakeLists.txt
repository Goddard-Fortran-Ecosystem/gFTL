install (DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/ DESTINATION "${dest}/include/shared"
  PATTERN *.inc
  PATTERN CMakeLists.txt EXCLUDE
  )

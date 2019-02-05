add_subdirectory (templates)
add_subdirectory (types)

file (COPY DIRECTORY . DESTINATION . FILES_MATCHING PATTERN "*.inc")
install (DIRECTORY ${CMAKE_CURRENT_BINARY_DIR} DESTINATION .  FILES_MATCHING PATTERN "*.inc")

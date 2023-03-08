cmake_minimum_required(VERSION 3.0)
if ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -assume realloc_lhs")
endif()
#add_subdirectory(v1)
add_subdirectory(v2)


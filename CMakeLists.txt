cmake_minimum_required(VERSION 2.8)
if(COMMAND cmake_policy)
  cmake_policy(SET CMP0003 NEW)
endif(COMMAND cmake_policy)

set(CMAKE_Fortran_COMPILER ${FC})

project (FTL)
enable_language (Fortran)

add_subdirectory(include)
add_subdirectory(examples)

if(EXISTS ${PFUNIT})
   add_subdirectory(tests)
endif()

cmake_minimum_required(VERSION 2.8)
if(COMMAND cmake_policy)
  cmake_policy(SET CMP0003 NEW)
endif(COMMAND cmake_policy)

set(PFUNIT "" CACHE PATH "(optional) path to installed testing framework")
if (PFUNIT)
  set(CMAKE_Fortran_COMPILER ${FC})
  project (FTL Fortran)
else ()
  project (FTL NONE)
endif()

add_subdirectory(include)
add_subdirectory(examples)

if(PFUNIT)
   add_subdirectory(tests)
endif()

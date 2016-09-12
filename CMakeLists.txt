cmake_minimum_required(VERSION 2.8)
if(COMMAND cmake_policy)
  cmake_policy(SET CMP0003 NEW)
endif(COMMAND cmake_policy)

set(CMAKE_Fortran_COMPILER ${FC})

project (FTL)
enable_language (Fortran)

message("${CMAKE_Fortran_COMPILER_ID}")
if ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel")
#  set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -O0 -traceback -save-temps -check uninit")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O0 -traceback -save-temps -check uninit -check bounds -g -warn unused")
#  set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -Ofast")
elseif ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O0 -fbacktrace -fcheck=bounds -fcheck=pointer -fcheck=mem -g")
elseif ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "NAG")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O0 -g -gline -C=dangling")
endif ()

include (cmake_utils/CheckCompilerCapabilities.cmake)

add_subdirectory(include)
add_subdirectory(examples)

if(EXISTS ${PFUNIT})
   add_subdirectory(tests)
endif()

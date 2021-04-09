# Compiler specific flags for Intel Fortran compiler

set(no_optimize "-O0")
set(check_all "-C=all")
set(cpp "-fpp")


set(CMAKE_Fortran_FLAGS_DEBUG  "${no_optimize}")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
set(CMAKE_Fortran_FLAGS "-g ${cpp} ${traceback} ${check_all}")

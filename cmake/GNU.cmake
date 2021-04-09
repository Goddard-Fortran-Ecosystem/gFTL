# Compiler specific flags for Intel Fortran compiler

set(no_optimize "-O0")
set(check_all "-fcheck=bounds -fcheck=pointer -fcheck=mem ")
set(traceback "-fbacktrace")
set(cpp "-cpp")


set(CMAKE_Fortran_FLAGS_DEBUG  "${no_optimize} ${check_all} ${traceback}")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
set(CMAKE_Fortran_FLAGS "-g ${cpp} -ffree-line-length-none")

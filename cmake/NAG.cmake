# Compiler specific flags for NAG Fortran compiler

set (no_optimize "-O0")
set (check_all "-C=all -nocheck_modtime")
set (cpp "-fpp")
set (suppress_fpp_warnings "-w")

set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g ${cpp}")
set (CMAKE_Fortran_FLAGS_DEBUG  "${no_optimize} ${traceback} ${check_all}")
set (CMAKE_Fortran_FLAGS_RELEASE "-O3")

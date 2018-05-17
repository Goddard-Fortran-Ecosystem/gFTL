# Compiler specific flags for Intel Fortran compiler

set(no_optimize "-O0")
#set(check_all "-C=all")
set(check_all "-C=array -C=alias -C=bits -C=calls -C=do -C=intovf -C=present -C=pointer")
set(cpp "-fpp")
set(suppress_fpp_warnings "-w")

set(CMAKE_Fortran_FLAGS_DEBUG  "${no_optimize}")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
set(CMAKE_Fortran_FLAGS "-g ${cpp} ${traceback} ${check_all}")

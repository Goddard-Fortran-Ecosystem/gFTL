# Compiler specific flags for Cray Fortran compiler

set(no_optimize "-O0 -g")
#set(check_all "-eC -eD -ec -en -eI -ea -g -G0")
#set(traceback "-fbacktrace")
set(cpp "-eF")


set(CMAKE_Fortran_FLAGS_DEBUG  "${cpp} ${no_optimize} ${check_all} ${traceback}")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3 ${cpp}")
#set(CMAKE_Fortran_FLAGS "${cpp} -ffree-line-length-none")

# Compiler specific flags for LFortran compiler

set(cpp "--cpp")

set(CMAKE_Fortran_FLAGS_DEBUG  "-g -O0 ${cpp}")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3 ${cpp}")
set(CMAKE_Fortran_FLAGS "${cpp}")

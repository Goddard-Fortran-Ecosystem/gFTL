# Compiler specific flags for LFortran compiler

# LFortran support came in CMake 3.31
cmake_minimum_required(VERSION 3.31)

set(cpp "--cpp")

set(CMAKE_Fortran_FLAGS_DEBUG  "-g -O0 ${cpp}")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3 ${cpp}")
set(CMAKE_Fortran_FLAGS "${cpp}")

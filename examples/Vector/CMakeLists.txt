cmake_minimum_required(VERSION 2.8)

include_directories("$ENV{FTL}/include")
#add_executable(VecIntAndReal.x EXCLUDE_FROM_ALL VecIntAndReal.F90)
#add_executable(VecMyType.x EXCLUDE_FROM_ALL VecMyType.F90)
#add_executable(VecMyPolyPtr.x EXCLUDE_FROM_ALL VecMyPolyPtr.F90)
add_executable(VecIntAndReal.x VecIntAndReal.F90)
add_executable(VecMyType.x VecMyType.F90)
add_executable(VecMyPolyPtr.x VecMyPolyPtr.F90)

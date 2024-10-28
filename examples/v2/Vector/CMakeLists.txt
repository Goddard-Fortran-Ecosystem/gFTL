cmake_minimum_required(VERSION 3.0)

project(VectorExamples Fortran)

find_package(GFTL REQUIRED)

add_executable(VecIntAndReal.x VecIntAndReal.F90)
add_executable(VecMyType.x VecMyType.F90)
add_executable(VecMyPolyPtr.x VecMyPolyPtr.F90)

target_link_libraries(VecIntAndReal.x GFTL::gftl-v2)
target_link_libraries(VecMyType.x GFTL::gftl-v2)
target_link_libraries(VecMyPolyPtr.x GFTL::gftl-v2)

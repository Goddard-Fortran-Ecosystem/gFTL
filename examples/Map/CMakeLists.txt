cmake_minimum_required(VERSION 2.8)

include_directories("$ENV{FTL}/include")
add_executable(CIKey.x CaseInsensitiveKey.F90)
add_executable(StringPoly.x StringPoly.F90)
add_executable(StringArray.x StringArray.F90)
add_executable(mapmap.x Mapsmap.F90)

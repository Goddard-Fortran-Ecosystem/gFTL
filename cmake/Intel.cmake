# Compiler specific flags for Intel Fortran compiler

if(WIN32)
  set(no_optimize "-Od")
  set(check_all "-check:all)
  set(debug_info "-Zi")
  set(save_temps "-Qsave-temps")
  set(disable_warning_for_long_names "-Qdiag-disable:5462")
  set(cpp "-fpp")
else()
  set(no_optimize "-O0")
  set(check_all "-check all,noarg_temp_created")
  set(debug_info "-g")
  set(save_temps "-save-temps")
  set(disable_warning_for_long_names "-diag-disable 5462")
  set(cpp "-cpp")
endif()
  

set(traceback "-traceback")


set(CMAKE_Fortran_FLAGS_DEBUG  "${no_optimize} ${check_all} ${traceback} ${save_temps}")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
set(CMAKE_Fortran_FLAGS "${debug_info} ${cpp} ${traceback} ${check_all} ${disable_warning_for_long_names} ${save_temps}")

add_definitions(-D_INTEL)

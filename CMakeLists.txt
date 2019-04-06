cmake_minimum_required (VERSION 3.0)
if (COMMAND cmake_policy)
  cmake_policy (SET CMP0003 NEW)
endif (COMMAND cmake_policy)

set (PFUNIT_DIR "" CACHE PATH "(optional) path to installed testing framework")
if (PFUNIT_DIR)
  set (LANGUAGES Fortran)
else ()
  set (LANGUAGES NONE)
endif ()

project (GFTL
  VERSION 1.1.3
  LANGUAGES ${LANGUAGES}
  )

if (PFUNIT_DIR)
  set (PFUNIT ${PFUNIT_DIR})
  set (CMAKE_Fortran_COMPILER ${FC})
  set (CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${GFTL_SOURCE_DIR}/cmake_utils")
  include (${CMAKE_Fortran_COMPILER_ID} RESULT_VARIABLE found)
  include (CheckFortranSource)
  include (CheckCompilerCapabilities)

  enable_testing()
  if (NOT TARGET (tests))
    add_custom_target(tests COMMAND ${CMAKE_CTEST_COMMAND})
  endif ()
endif ()

if (CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set (CMAKE_INSTALL_PREFIX "${CMAKE_BINARY_DIR}/installed" CACHE PATH "..." FORCE)
  message("Setting default install prefix to ${CMAKE_INSTALL_PREFIX}.")
  message("Override with -DCMAKE_INSTALL_PREFIX=<path>.")
endif ()


add_subdirectory (include)
add_subdirectory (examples)

if (PFUNIT_DIR)
   add_subdirectory(tests)
endif ()

configure_file(GFTLConfig.cmake.in ${CMAKE_CURRENT_BINARY_DIR}/GFTLConfig.cmake @ONLY)
configure_file(GFTLConfig-version.cmake.in ${CMAKE_CURRENT_BINARY_DIR}/GFTLConfig-version.cmake @ONLY)
configure_file(GFTL.mk.in ${CMAKE_CURRENT_BINARY_DIR}/GFTL.mk @ONLY)

set (top_dir "GFTL-${GFTL_VERSION_MAJOR}.${GFTL_VERSION_MINOR}")
install (
  FILES ${CMAKE_CURRENT_BINARY_DIR}/GFTLConfig.cmake ${CMAKE_CURRENT_BINARY_DIR}/GFTLConfig-version.cmake
  DESTINATION "${top_dir}/cmake"
  )
install (
  FILES ${CMAKE_CURRENT_BINARY_DIR}/GFTL.mk 
  DESTINATION "${top_dir}/include"
  )

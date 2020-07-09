cmake_minimum_required (VERSION 3.0)
if (COMMAND cmake_policy)
  cmake_policy (SET CMP0003 NEW)
endif (COMMAND cmake_policy)

project (gFTL
  VERSION 2.0.0
  LANGUAGES NONE)

if (CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set (CMAKE_INSTALL_PREFIX "${CMAKE_BINARY_DIR}/installed" CACHE PATH "..." FORCE)
  message("-- Setting default install prefix to ${CMAKE_INSTALL_PREFIX}.")
  message("   Override with -DCMAKE_INSTALL_PREFIX=<path>.")
endif ()

# For BSD systems use GNU m4
find_program( M4 NAMES "gm4" "m4")
if( NOT M4 )
  message( SEND_ERROR "m4 program not found" )
endif()

# Some preprocessing tests can be run without the
# Fortran testing framework.
enable_testing()


# v2
add_subdirectory (include)

# v1 legacy
add_subdirectory (include-v1)
add_subdirectory (examples)

# No Fortran compiler is required to build gFTL, but the tests do
# require Fortran.  So we check for that before searching for the
# testing framework.

include(CheckLanguage)
check_language (Fortran)
if (CMAKE_Fortran_COMPILER)
  enable_language(Fortran)

  find_package (PFUNIT 4.1 QUIET)
  if (PFUNIT_FOUND)
    message("-- Detecting pFUnit: ${PFUNIT_DIR}")
    project (gFTL-TEST
      VERSION ${gFTL_VERSION}
      LANGUAGES Fortran
      )
    
    set (CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${gFTL_SOURCE_DIR}/cmake")
    include (${CMAKE_Fortran_COMPILER_ID} RESULT_VARIABLE found)
    include (CheckFortranSource)
    include (CheckCompilerCapabilities)

    if (NOT TARGET tests)
      add_custom_target(tests COMMAND ${CMAKE_CTEST_COMMAND})
    endif ()
  
    add_subdirectory(tests EXCLUDE_FROM_ALL)
  endif ()
endif ()

# The following is needed for external projects using *nix make when
# parent project builds gFTL as a subproject.
set (top_dir "GFTL-${GFTL_VERSION_MAJOR}.${GFTL_VERSION_MINOR}")
set (GFTL_TOP_DIR "${CMAKE_INSTALL_PREFIX}/${top_dir}" CACHE PATH "")

include(CMakePackageConfigHelpers)
configure_package_config_file(GFTLConfig.cmake.in GFTLConfig.cmake
  INSTALL_DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/GFTLConfig.cmake
  )
write_basic_package_version_file(GFTLConfig-version.cmake
  VERSION ${PACKAGE_VERSION}
  COMPATIBILITY SameMajorVersion
  )

install (
  FILES  ${CMAKE_CURRENT_BINARY_DIR}/gFTLConfig.cmake ${CMAKE_CURRENT_BINARY_DIR}/gFTLConfig-version.cmake
  DESTINATION ${top_dir}/${cmake_dir}
  )

configure_file(GFTL.mk.in ${CMAKE_CURRENT_BINARY_DIR}/GFTL.mk @ONLY)

install (
  FILES ${CMAKE_CURRENT_BINARY_DIR}/gFTL.mk 
  DESTINATION "${top_dir}/include-v1"
  )

install(EXPORT GFTL
  FILE GFTLTargets.cmake
  NAMESPACE GFTL::
  DESTINATION "${top_dir}/cmake"
  )
export(EXPORT GFTL
  FILE "${CMAKE_CURRENT_BINARY_DIR}/GFTLTargets.cmake"
  NAMESPACE GFTL::
  )


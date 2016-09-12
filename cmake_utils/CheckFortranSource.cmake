
#.rst:
# CheckFortranSourceCompiles
# --------------------------
#
# Check if given Fortran source compiles and links into an executable::
#
#   CHECK_Fortran_SOURCE_COMPILES(<code> <var> [FAIL_REGEX <fail-regex>])
#
# The arguments are:
#
# ``<code>``
#   Source code to try to compile.  It must define a PROGRAM entry point.
# ``<var>``
#   Variable to store whether the source code compiled.
#   Will be created as an internal cache variable.
# ``<fail-regex>``
#   Fail if test output matches this regex.
#
# The following variables may be set before calling this macro to modify
# the way the check is run::
#
#   CMAKE_REQUIRED_FLAGS = string of compile command line flags
#   CMAKE_REQUIRED_DEFINITIONS = list of macros to define (-DFOO=bar)
#   CMAKE_REQUIRED_INCLUDES = list of include directories
#   CMAKE_REQUIRED_LIBRARIES = list of libraries to link
#   CMAKE_REQUIRED_QUIET = execute quietly without messages
#
#=============================================================================
# CMake - Cross Platform Makefile Generator
# Copyright 2000-2014 Kitware, Inc.
# Copyright 2000-2011 Insight Software Consortium
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#
# * Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.
# 
# * Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.
# 
# * Neither the names of Kitware, Inc., the Insight Software Consortium,
# nor the names of their contributors may be used to endorse or promote
# products derived from this software without specific prior written
# permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# =============================================================================

# This file was modified my T. Clune to enable directly using add_definition()
# based upon the results of the try().

# Two functions are provided:  CHECK_Fortran_SOURCE_COMPILE and CHECK_Fortran_SOURCE_RUN)


macro (CHECK_Fortran_SOURCE_COMPILE file var)

  if (NOT CMAKE_REQUIRED_QUIET)
    message (STATUS "Performing Test ${var}")
  endif ()

  try_compile (
    ${var}
    ${CMAKE_BINARY_DIR}
    ${file}
    )

  if (${var})
    if (NOT CMAKE_REQUIRED_QUIET)
      message(STATUS "Performing Test ${var}: SUCCESSS")
    endif ()

    add_definitions(-D${var})

  else ()

    if (NOT CMAKE_REQUIRED_QUIET)
      message(STATUS "Performing Test ${var}: FAILURE")
    endif ()

  endif ()

endmacro (CHECK_Fortran_SOURCE_COMPILE)


macro (CHECK_Fortran_SOURCE_RUN file var)

  if (NOT CMAKE_REQUIRED_QUIET)
    message (STATUS "Performing Test ${var}")
  endif ()

  try_run (
    code_runs
    code_compiles
    ${CMAKE_BINARY_DIR}
    ${file}
    )

  if (${code_compiles})
    if (${code_runs} EQUAL 0)

      if (NOT CMAKE_REQUIRED_QUIET)
	message (STATUS "Performing Test ${var}: SUCCESS")
      endif ()

      add_definitions(-D${var})

      set (${var} 1)

    else ()

      if (NOT CMAKE_REQUIRED_QUIET)
	message (STATUS "Performing Test ${var}: RUN FAILURE")
      endif ()

    endif ()

  else ()

      if (NOT CMAKE_REQUIRED_QUIET)
	message (STATUS "Performing Test ${var}: BUILD FAILURE")
      endif ()

  endif()

endmacro (CHECK_Fortran_SOURCE_RUN)

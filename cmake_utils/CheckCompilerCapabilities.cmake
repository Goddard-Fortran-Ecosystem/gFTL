include (cmake_utils/CheckFortranSource.cmake)

CHECK_Fortran_SOURCE_COMPILE (
  ${CMAKE_SOURCE_DIR}/cmake_utils/pointerToFixedLengthString.F90
  SUPPORT_FOR_POINTERS_TO_FIXED_LENGTH_STRINGS
)

CHECK_Fortran_SOURCE_RUN (
  ${CMAKE_SOURCE_DIR}/cmake_utils/pointerToDeferredLengthString.F90
  SUPPORT_FOR_POINTERS_TO_DEFERRED_LENGTH_STRINGS
)

CHECK_Fortran_SOURCE_RUN (
  ${CMAKE_SOURCE_DIR}/cmake_utils/supportsInt64.F90
  SUPPORT_FOR_INT64
)

CHECK_Fortran_SOURCE_COMPILE (
  ${CMAKE_SOURCE_DIR}/cmake_utils/supportsQuadPrecision.F90
  SUPPORT_FOR_QUAD_PRECISION
)


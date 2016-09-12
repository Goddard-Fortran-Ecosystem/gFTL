set (SRCS
  IntegerVector.F90
  RealVector.F90
  LogicalVector.F90

  Real64Vector.F90
  Character17Vector.F90
  DeferredLengthStringVector.F90

  IntegerPtrVector.F90
  Integer1dVector.F90

  Foo.F90
  ChildOfFoo.F90
  FooVector.F90
)

set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -O0 -traceback -assume realloc_lhs")

include_directories (${FTL_SOURCE_DIR}/include)
include_directories ($ENV{PFUNIT}/mod)

add_library(vectorSUT STATIC ${SRCS})

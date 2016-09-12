set (SRCS
  Foo.F90
  ChildOfFoo.F90
  pFUnitSupplement.F90
  castFromInteger.F90
)

include_directories (${FTL_SOURCE_DIR}/include)
include_directories (${FTL_BINARY_DIR}/include)
include_directories (${FTL_BINARY_DIR}/mod)
include_directories ($ENV{PFUNIT}/mod)

add_library(shared ${SRCS})

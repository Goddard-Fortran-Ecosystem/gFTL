set (SRCS
  Foo.F90
  ChildOfFoo.F90
  pFUnitSupplement.F90
)

include_directories (${gFTL_SOURCE_DIR}/include)
include_directories (${gFTL_BINARY_DIR}/include)
include_directories (${PFUNIT}/mod)

add_library(shared ${SRCS})

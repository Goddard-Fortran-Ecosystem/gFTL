set (SRCS
  Foo.F90
  ChildOfFoo.F90
  pFUnitSupplement.F90
)

add_library(shared ${SRCS})
target_include_directories (shared PRIVATE ${PFUNIT}/mod)
target_include_directories (shared PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
target_link_libraries(shared gftl pfunit)


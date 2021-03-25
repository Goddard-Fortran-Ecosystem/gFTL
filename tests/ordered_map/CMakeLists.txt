find_program(M4 m4)

# Note that a set of elements of type LOGICAL is not
# supported.
set (keys
  integer
#  character_len17
#  deferred_string
#  Foo
#  logical
#  integer_shape_3
#  real_rank_2_deferred_shape
  )

set (types
  integer
#  character_len17
#  deferred_string
#  Foo
#  logical
#  unlimited
#  integer_shape_3
#  real_rank_2_deferred_shape
  )


set (src ${CMAKE_CURRENT_SOURCE_DIR})
set (bin ${CMAKE_CURRENT_BINARY_DIR})
set (incs "../shared/include")
foreach (key ${keys})
  foreach (T ${types})
    set (sut ${key}${T}OrderedMap.F90)

    m4_preprocess2 (${key} ${T} ${src}/OrderedMap.m4 ${sut})
    m4_preprocess2 (${key} ${T} ${src}/Test_OrderedMap.m4 Test_${key}${T}OrderedMap.pf)

    add_pfunit_ctest (test_${key}${T}_ordered_map.x
      TEST_SOURCES ${bin}/Test_${key}${T}OrderedMap.pf
      OTHER_SOURCES ${sut}
      LINK_LIBRARIES test_shared gftl-v2
      )
    add_dependencies (tests test_${key}${T}_ordered_map.x)

  endforeach ()
endforeach ()
  

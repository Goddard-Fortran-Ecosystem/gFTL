# Note that a set of elements of type LOGICAL is not
# supported.
set (keys
  integer
  integer32
  character_len17
  deferred_string
  Foo
  )

set (types
  integer
  character_len17
  deferred_string
  Foo
  logical
  unlimited
  integer_shape_3
  real_rank_2_deferred_shape
  )


set (src ${CMAKE_CURRENT_SOURCE_DIR})
set (bin ${CMAKE_CURRENT_BINARY_DIR})
set (incs "../shared/include")
foreach (key ${keys})
  foreach (T ${types})

    set (sut ${key}${T}Map.F90)
    m4_preprocess2 (${key} ${T} ${src}/Map.m4 ${sut})
    m4_preprocess2 (${key} ${T} ${src}/Test_Map.m4 Test_${key}${T}Map.pf)
    m4_preprocess2 (${key} ${T} ${src}/Test_MapAlgorithms.m4 Test_${key}${T}MapAlg.pf)

    add_pfunit_ctest (test_${key}${T}_map.x
      TEST_SOURCES ${bin}/Test_${key}${T}Map.pf ${bin}/Test_${key}${T}MapAlg.pf
      OTHER_SOURCES ${sut}
      LINK_LIBRARIES test_shared gftl-v2
    )
    add_dependencies (tests test_${key}${T}_map.x)

    # variant implementation
    set (alt_sut ${key}${T}alt_Map.F90)
    m4_preprocess2 (${key} ${T} ${src}/alt_Map.m4 ${alt_sut})
    m4_preprocess2 (${key} ${T} ${src}/Test_alt_Map.m4 Test_${key}${T}alt_Map.pf)
    m4_preprocess2 (${key} ${T} ${src}/Test_alt_MapAlgorithms.m4 Test_${key}${T}alt_MapAlg.pf)

    add_pfunit_ctest (test_${key}${T}_alt_map.x
      TEST_SOURCES ${bin}/Test_${key}${T}alt_Map.pf ${bin}/Test_${key}${T}alt_MapAlg.pf
      OTHER_SOURCES ${alt_sut}
      LINK_LIBRARIES test_shared gftl-v2
      )
    add_dependencies (tests test_${key}${T}_alt_map.x)

  endforeach ()
endforeach ()
  

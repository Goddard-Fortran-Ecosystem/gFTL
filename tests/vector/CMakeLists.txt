#add_subdirectory(sut)

find_program(M4 m4)

set (types
  integer

  )

foreach (T ${types})
  set (sut ${T}Vector)
  set (test Test_${T}Vector)
  set (src ${CMAKE_CURRENT_SOURCE_DIR})
  set (bin ${CMAKE_CURRENT_BINARY_DIR})
    
  add_custom_command (
    OUTPUT ${bin}/${sut}.F90 ${bin}/${test}.pf  ${bin}/${test}Iterator.pf ${bin}/${test}RIterator.pf
    COMMAND ${M4} -s -D_type=${T} ${src}/Vector.m4  > ${sut}.F90
    COMMAND ${M4} -s -D_type=${T} ${src}/Test_Vector.m4 > ${test}.pf
    COMMAND ${M4} -s -D_type=${T} ${src}/Test_VectorIterator.m4 > ${test}Iterator.pf
    COMMAND ${M4} -s -D_type=${T} ${src}/Test_VectorRIterator.m4 > ${test}RIterator.pf
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS ${src}/Vector.m4 ${src}/Test_Vector.m4 ${src}/Test_VectorIterator.m4 ${src}/Test_VectorRIterator.m4 ${src}/${T}.inc 
    )

  add_pfunit_ctest (${test}.x
    TEST_SOURCES ${bin}/${test}.pf ${bin}/${test}Iterator.pf ${bin}/${test}RIterator.pf
    OTHER_SOURCES ${bin}/${sut}.F90
    LINK_LIBRARIES gftl-v2
    )

  add_dependencies (tests ${test}.x)

endforeach ()


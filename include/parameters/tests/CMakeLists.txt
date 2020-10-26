set (expected "expected_integer.inc")
set (found "found_integer.inc")


find_program(CPP cpp)

set (types
  logical
  integer
  integer32
  real
  complex
  fixed_string
  deferred_string
  unlimited

# Combos
  integer_rank_1
  integer32_rank_2
  integer_shape_1d
  integer32_shape_2d

# Derived
  Foo
  FooPoly
  kitchen_sink
  )

set(actuals)
foreach(type ${types})

  set (source ${CMAKE_CURRENT_SOURCE_DIR}/${type}.inc)
  set (expected ${CMAKE_CURRENT_SOURCE_DIR}/expected_${type}.txt)
  set (tmp tmp_${type}.txt)
  set (tmp2 tmp2_${type}.txt)
  set (actual actual_${type}.txt)

  find_program(AWK awk)
  configure_file (filter.x.in filter.x @ONLY)
  find_program(FILTER filter.x ${CMAKE_CURRENT_BINARY_DIR})

  add_custom_command (
    OUTPUT ${actual}
    COMMAND ${CPP} -I${gFTL_SOURCE_DIR}/include -I${gFTL_BINARY_DIR}/include -I${gFTL_SOURCE_DIR}/include ${source} > ${tmp}
    # Strip all empty lines and comments
    COMMAND ${FILTER} ${tmp} ${actual}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS ${type}.inc
    )

  list(APPEND actuals ${actual})

  add_test(
    NAME test_derived_${type}
    COMMAND diff ${expected} ${actual}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    )

endforeach()

add_custom_target (test_generated_incs ALL DEPENDS ${actuals})
add_dependencies (test_generated_incs m4_type_includes)

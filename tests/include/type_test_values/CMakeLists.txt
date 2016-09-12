set (src ${CMAKE_CURRENT_SOURCE_DIR})
set (bin ${CMAKE_CURRENT_BINARY_DIR})

set (template_params type key value)
set (macro_files
  integer
  real
  real32
  real64
  complex
  complex64
  complex128
  logical
  character17
  deferredLengthString
  unlimitedPoly
  unlimitedPolyPtr

  integer1d
  integer2d
  integer2d_fixedExtents

  integerPtr
  integer2dPtr
  integerAlloc

  Foo
  FooPtr
  FooPoly
  FooPolyPtr

  )

# Empty list - will append in loop below
set (depends)

foreach( macro_file ${macro_files} )
  foreach( param ${template_params} )
    set( infile ${src}/${macro_file}.m4 )
    if (${param} STREQUAL type)
      set( outfile ${macro_file}.inc )
    else()
      set( outfile ${param}_${macro_file}.inc )
    endif ()

# Use relative path for outfile so that CMake correctly
# detects the need to generate include files.

    add_custom_command(
	OUTPUT ${outfile}
	COMMAND m4 -s -Dparam=${param} -I${CMAKE_SOURCE_DIR}/include/templates < ${infile} > ${outfile}
	WORKING_DIRECTORY ${bin}
	DEPENDS ${infile}
	)

    list( APPEND depends ${outfile} )

  endforeach()
endforeach()

add_custom_target( type_test_values
  DEPENDS ${depends}
  )

set(src ${CMAKE_CURRENT_SOURCE_DIR})
set(bin ${CMAKE_CURRENT_BINARY_DIR})

# Need to create a 2D array of include files.
# First axis is {type,key,value}
set (template_params type key value)

# Second axis is role
set (macro_files
  template_macros
  template_macros_undefs
  testing_macros
  set_use_tokens
  use_tokens_undef
  macros_undefs)
set(depends)

foreach( macro_file ${macro_files} )
  foreach( param ${template_params} )
    set( infile ${src}/${macro_file}.m4 )
    set( outfile ${param}_${macro_file}.inc )

# Use relative path for outfile so that CMake correctly
# detects the need to generate include files.

    add_custom_command(
	OUTPUT ${outfile}
	COMMAND m4 -s -Dparam=${param} -I${src} < ${infile} > ${outfile}
	WORKING_DIRECTORY ${bin}
	DEPENDS ${infile}
	)

    list( APPEND depends ${outfile} )

  endforeach()
endforeach()

add_custom_target( template_macros ALL 
  DEPENDS ${depends}
  )




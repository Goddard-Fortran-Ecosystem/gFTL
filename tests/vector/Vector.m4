changecom()

module _type()Vector_mod
   use, intrinsic :: iso_fortran_env
   ifelse(_type(),`Foo',`  use Foo_mod')
   ifelse(_type(),`FooPoly',`   use Foo_mod')

#include "`'_type()`'.inc"
#include "vector/template.inc"

end module _type()Vector_mod


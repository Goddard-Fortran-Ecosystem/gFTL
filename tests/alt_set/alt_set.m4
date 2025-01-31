changecom()

module _type()_alt_set_mod
   use, intrinsic :: iso_fortran_env
   ifelse(_type(),`Foo',`  use Foo_mod')

#include "`'_type()`'.inc"
#include "alt_set/template.inc"

end module _type()_alt_set_mod


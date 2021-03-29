changecom()

module _type()Set_mod
   use, intrinsic :: iso_fortran_env
   ifelse(_type(),`Foo',`  use Foo_mod')

#include "`'_type()`'.inc"
#include "set/template.inc"

end module _type()Set_mod


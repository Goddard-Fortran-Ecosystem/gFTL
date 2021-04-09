changecom()

module _type()Deque_mod
   use, intrinsic :: iso_fortran_env
   ifelse(_type(),`Foo',`  use Foo_mod')
   ifelse(_type(),`FooPoly',`   use Foo_mod')

#include "`'_type()`'.inc"
#include "deque/template.inc"

end module _type()Deque_mod


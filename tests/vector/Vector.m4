changecom()

module _type()Vector_mod
   ifelse(_type(),`Foo',`  use Foo_mod')

#include "`'_type()`'.inc"
#include "vector/template.inc"

end module _type()Vector_mod


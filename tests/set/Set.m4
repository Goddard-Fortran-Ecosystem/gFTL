changecom()

module _type()Set_mod
   ifelse(_type(),`Foo',`  use Foo_mod')

#include "`'_type()`'.inc"
#include "set/template.inc"

end module _type()Set_mod


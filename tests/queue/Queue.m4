changecom()

module _type()Queue_mod
   ifelse(_type(),`Foo',`  use Foo_mod')
   ifelse(_type(),`FooPoly',`   use Foo_mod')

#include "`'_type()`'.inc"
#include "queue/template.inc"

end module _type()Queue_mod


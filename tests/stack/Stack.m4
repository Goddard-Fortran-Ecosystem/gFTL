changecom()

module _type()Stack_mod
   ifelse(_type(),`Foo',`  use Foo_mod')
   ifelse(_type(),`FooPoly',`   use Foo_mod')

#include "`'_type()`'.inc"
#include "stack/template.inc"

end module _type()Stack_mod


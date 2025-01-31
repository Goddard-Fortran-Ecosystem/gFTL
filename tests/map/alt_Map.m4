changecom()

module _key()`'_type()alt_Map_mod
   use, intrinsic :: iso_fortran_env
   ifelse(_type(),`Foo',`  use Foo_mod',_key(),`Foo',`use Foo_mod')

#define USE_ALT_SET
#include "`'_key()`'_Key.inc"
#include "`'_type()`'_T.inc"
#include "map/template.inc"

end module _key()`'_type()alt_Map_mod

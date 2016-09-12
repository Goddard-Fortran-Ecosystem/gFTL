changecom()

define(`suffix',ifelse(format(),free,,_fixedFormat))

ifelse(ALT,alt,
`
#define _alt
')

      module key()`'value()`'ALT()Map`'suffix()_mod
#include "types/key_`'key().inc"
#include "types/value_`'value().inc"
#include "templates/map.inc"
#include "templates/all_macros_undefs.inc"
      end module key()`'value()`'ALT()Map`'suffix()_mod

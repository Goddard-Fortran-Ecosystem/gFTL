      module IntegerFixedFormatSet_mod
#include "types/integer.inc"
#include "templates/altset.inc"
      end module IntegerFixedFormatSet_mod

! A second module checks that all macros are cleared from the 1st case
      module Other_mod
#include <types/integer.inc>
#include <templates/altset.inc>
      end module Other_mod

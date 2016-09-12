#include "templates/tmplbase.inc"
      private

#ifdef _set
#  define __set _set
#else
#  define __set set
#endif
#define __container_prefix s_

#ifdef _iterator
#  define __iterator _iterator
#else
#  define __iterator __QUOTE(__set)__QUOTE(Iterator)
#endif
      public :: __set
      public :: __iterator

#include "templates/type_set_use_tokens.inc"

#include "set_decl.inc"

      contains

#include "set_impl.inc"

#include "templates/type_use_tokens_undef.inc"

#include "templates/tmpltail.inc"
#include "templates/all_macros_undefs.inc"


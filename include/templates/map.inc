#include "templates/tmplbase.inc"
      private

#ifdef _map
#  define __map _map
#else
#  define __map map
#endif

#ifdef _iterator
#  define __miterator _iterator
#else
#  define __miterator __QUOTE(__map)__QUOTE(Iterator)
#endif
#define __siterator s_iterator
#define __set set


#ifdef _pair
#  define __pair _pair
#else
#  define __pair pair
#endif

      public :: __map
      public :: __miterator
      public :: __pair


#ifdef _alt
! Implementation alse uses vector of <integer(kind=SIZE_KIND)> for
! indices.
#  define __vector iVector
#  define __container_prefix ti_
#  define __use_type integer(kind=SIZE_KIND)
#  include "vector_decl.inc"
#  include "templates/type_use_tokens_undef.inc"
#  undef __vector
#  undef __container_prefix
#  endif

#include "templates/key_set_use_tokens.inc"
#include "templates/value_set_use_tokens.inc"

#define __container_prefix m_
#include "pair_decl.inc"
#undef __container_prefix

#define _type type(__pair)
#ifdef _pair_allocatable      
#define _allocatable
#endif      

#include "templates/type_set_use_tokens.inc"

#ifdef _alt
! Implementation uses a vector of <_type>
#  define __vector tVector
#  define __container_prefix tt_

#  include "vector_decl.inc"
#  undef __vector
#  undef __container_prefix
#  endif

#include "templates/type_use_tokens_undef.inc"

! Set/Map rely on additional comparison properties.
#define _equal_defined
#define _EQ(x,y) (x .sameKey. y)
#define _LESS_THAN(x,y) __KEY_LESS_THAN(x%key,y%key)

#include "templates/key_template_macros.inc"
#ifdef __key_needs_default_compare
#  define __set_needs_default_compare
#endif
#include "templates/key_template_macros_undefs.inc"

#include "templates/type_set_use_tokens.inc"

#define __container_prefix s_
#define __iterator __siterator

#ifdef __set_needs_default_compare
#  define __type_needs_default_compare
#endif

#ifdef _alt
#  include "altSet_decl.inc"
#else
#  include "set_decl.inc"
#endif

#undef __iterator
#undef __container_prefix

#define __container_prefix m_
#define __iterator __miterator
#include "map_decl.inc"
#undef __iterator
#undef __container_prefix

      contains

! Token is cleared out with the above include.  Need to redefine.
#include "templates/key_set_use_tokens.inc"
#include "templates/value_set_use_tokens.inc"
#include "templates/type_set_use_tokens.inc"

#define __compare_type __key_type
#define __compare_declare_dummy __key_declare_dummy
#define __compare_declare_component __key_declare_component
#define __COMPARE_ASSIGN(x,y) __KEY_ASSIGN(x,y)
#define __COMPARE_FREE(x) __KEY_FREE(x)

#ifdef __set_needs_default_compare
#  define __type_needs_default_compare
#endif

#define __container_prefix s_
#define __iterator __siterator
#  ifdef _alt
#    include "altSet_impl.inc"
#  else
#    include "set_impl.inc"
#  endif
#undef __container_prefix
#undef __iterator

#undef _equal_defined
#undef _EQ
#undef _LESS_THAN

#define __container_prefix m_
#define __iterator __miterator

#include "pair_impl.inc"
#include "map_impl.inc"

#undef __container_prefix
#undef __iterator

#include "templates/type_use_tokens_undef.inc"

#include "templates/type_set_use_tokens.inc"

#ifdef _alt
! vector<_type>
#  define __vector tVector
#  define __container_prefix tt_
#  include "vector_impl.inc"
#  undef __vector
#  undef __container_prefix
#endif

#include "templates/key_use_tokens_undef.inc"
#include "templates/value_use_tokens_undef.inc"
#include "templates/type_use_tokens_undef.inc"

#ifdef _alt
! vector <integer(kind=SIZE_KIND)>
#  define __vector iVector
#  define __container_prefix ti_
#  define __use_type integer(kind=SIZE_KIND)
#  include "vector_impl.inc"
#  include "templates/type_use_tokens_undef.inc"
#  undef __vector
#  undef __container_prefix
#endif

#undef __miterator
#undef __map
#undef __set
#undef __siterator
#undef __set_needs_default_compare
#include "templates/tmpltail.inc"
#include "templates/all_macros_undefs.inc"


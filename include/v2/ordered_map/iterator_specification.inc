   type :: __omap_iterator
      private
      type(__MANGLE(VectorIterator)) :: key_iter
      type(__omap), pointer :: reference
   contains
      procedure :: at => __MANGLE(iter_at)
      procedure :: first => __MANGLE(iter_first)
      procedure :: second => __MANGLE(iter_second)
      procedure :: next => __MANGLE(iter_next)
      procedure :: prev => __MANGLE(iter_prev)
   end type __omap_iterator


   interface operator(==)
      module procedure :: __MANGLE(iter_equal)
   end interface operator(==)

   interface operator(/=)
      module procedure :: __MANGLE(iter_not_equal)
   end interface operator(/=)

   interface advance
#ifndef  __gftl_disable_index_kind_overload
      module procedure __MANGLE(iter_advance_size_kind)
#endif
      module procedure __MANGLE(iter_advance_default)
   end interface advance

   interface begin
      module procedure __MANGLE(iter_begin)
   end interface begin

   interface end
      module procedure __MANGLE(iter_end)
   end interface end

   interface next
      module procedure __MANGLE(iter_next_1)
#ifndef  __gftl_disable_index_kind_overload
      module procedure __MANGLE(iter_next_n_size_kind)
#endif
      module procedure __MANGLE(iter_next_n_default)
   end interface next

   interface prev
      module procedure __MANGLE(iter_prev_1)
#ifndef __gftl_disable_index_kind_overload
      module procedure __MANGLE(iter_prev_n_size_kind)
#endif
      module procedure __MANGLE(iter_prev_n_default)
   end interface prev
   

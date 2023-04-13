

   type :: __set_iterator
      private
      type(__set), pointer :: tree => null()
      type(__set_node), pointer :: node => null()
   contains
      procedure :: of => __MANGLE(iter_of)
      procedure :: next => __MANGLE(iter_next)
      procedure :: prev => __MANGLE(iter_prev)
   end type __set_iterator

   interface operator(==)
      module procedure __MANGLE(iter_equal)
   end interface operator(==)

   interface operator(/=)
      module procedure __MANGLE(iter_not_equal)
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

   interface ftn_begin
      module procedure __MANGLE(iter_ftn_begin)
   end interface ftn_begin

   interface ftn_end
      module procedure __MANGLE(iter_ftn_end)
   end interface ftn_end

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
   

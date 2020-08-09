   type :: __map_iterator
      private
      type(__MANGLE(SetIterator)) :: set_iter
      class(__map), pointer :: reference
   contains
      procedure :: at => __MANGLE(iter_at)
      procedure :: first => __MANGLE(iter_first)
      procedure :: second => __MANGLE(iter_second)
      procedure :: next => __MANGLE(iter_next)
      procedure :: prev => __MANGLE(iter_prev)
   end type __map_iterator


#ifdef _alt
   interface __map
      module procedure __MANGLE(new_map_empty)
      module procedure __MANGLE(new_map_from_pair_array)
   end interface __map
#endif

   interface operator(==)
      module procedure :: __MANGLE(iter_equal)
   end interface operator(==)

   interface operator(/=)
      module procedure :: __MANGLE(iter_not_equal)
   end interface operator(/=)


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

   



   type :: __set_iterator
      private
      type(__set), pointer :: tree => null()
      type(__MANGLE(node)), pointer :: node => null()
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

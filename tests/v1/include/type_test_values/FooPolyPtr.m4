include(header.m4)
      use ChildOfFoo_mod
#define __PARAM()_CAST_FROM_INTEGER(var,val) var = Foo(val)

#define _base()_ONE Foo(1)
#define _base()_ONE_B Foo(-1)
#define _base()_TWO ChildOfFoo(2,3)
#define _base()_THREE Foo(3)
#define _base()_FOUR ChildOfFoo(4,5)
#define _base()_FIVE Foo(6)

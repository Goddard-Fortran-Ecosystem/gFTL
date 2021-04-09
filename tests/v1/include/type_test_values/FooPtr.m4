include(header.m4)

	use Foo_mod, only: assertEqual
#define __PARAM()_CAST_FROM_INTEGER(var,val) var = Foo(val)

#define _base()_ONE Foo(1)
#define _base()_ONE_B Foo(-1)
#define _base()_TWO Foo(2)
#define _base()_THREE Foo(3)
#define _base()_FOUR Foo(4)
#define _base()_FIVE Foo(5)

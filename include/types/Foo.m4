include(header.m4)

      use Foo_mod
#define _param() type(Foo)
#define _base()_equal_defined
#define _BASE()_LESS_THAN(x,y) (x%i < y%i)

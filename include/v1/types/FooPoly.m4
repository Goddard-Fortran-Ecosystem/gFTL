include(header.m4)

      use Foo_mod, only: Foo

#define _param() class(Foo)
#define _base()_allocatable
#define _base()_equal_defined
#define _BASE()_LESS_THAN(x,y) (x%i<y%i)

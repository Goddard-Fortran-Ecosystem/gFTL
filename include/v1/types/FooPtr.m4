include(header.m4)

      use Foo_mod, only: Foo
#define _param() type(Foo)
#define _base()_pointer
#define _base()_equal_defined

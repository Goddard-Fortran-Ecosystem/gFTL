include(header.m4)

   use ftl_LessThan_mod
   use iso_fortran_env, only: real64
#define _type complex(kind=real64)
#define _equal_defined
#define _LESS_THAN(x,y)  (x .lessThan. y)



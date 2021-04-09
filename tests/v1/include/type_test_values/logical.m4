include(header.m4)

#define __PARAM()_CAST_FROM_INTEGER(var,val) var = (mod(val,2) == 1)

#define _base()_ONE .true.
#define _base()_ONE_B .false.
#define _base()_TWO .false.
#define _base()_THREE .true.
#define _base()_FOUR .true.
#define _base()_FIVE .false.

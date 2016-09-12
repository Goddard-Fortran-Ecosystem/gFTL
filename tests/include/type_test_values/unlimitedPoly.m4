include(header.m4)

#define __PARAM()_CAST_FROM_INTEGER(var,val) call castToUnlimited(val, var)

#define _unlimited

#define _base()_ONE 1.
#define _base()_ONE_B -1.
#define _base()_TWO 2.
#define _base()_THREE 3.d0
#define _base()_FOUR (4.,5.)
#define _base()_FIVE 5

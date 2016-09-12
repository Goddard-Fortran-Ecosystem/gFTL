include(header.m4)
! This creates arrays of length 1 to 10
#define __BASE()_CAST_FROM_INTEGER(var,val) var = spread(val,1,mod(max(i,1),10)+1)

#define _base()_ONE ([1])
#define _base()_ONE_B ([-1])
#define _base()_TWO ([2,2])
#define _base()_THREE ([3,3,3])
#define _base()_FOUR ([4,4,4,4])
#define _base()_FIVE ([5,5,5,5,5])

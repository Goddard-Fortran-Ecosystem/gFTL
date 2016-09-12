include(header.m4)

#define __PARAM()_CAST_FROM_INTEGER(var,val) var = spread(spread(val,1,mod(i,3)+1),mod(i,2)+1)

! the following are only used for testing
#define _base()_ONE reshape([1,1],[1,2])
#define _base()_ONE_B reshape([-1,-1],[1,2])
#define _base()_TWO reshape([2,2],[1,2])
#define _base()_THREE reshape([3,3],[1,2])
#define _base()_FOUR reshape([4,4],[1,2])
#define _base()_FIVE reshape([5,5],[1,2])

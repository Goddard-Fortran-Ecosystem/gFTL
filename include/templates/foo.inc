!!!!! Begin user code !!!!!

module TypeAndContainer
   <USE statements>

#define T1 ...
#define T2 ...
#define pair Pair
!!!!! End user code !!!!!

#include "templates/pair/header.inc"

!!!!! Begin user code !!!!!
   <user specification>
!!!!! End user code !!!!!
   
#include "templates/pair/specification.inc"

!!!!! Begin user code !!!!!
contains
<user procedures>
!!!!! End user code !!!!!
  
#include "templates/pair/procedures.inc"
#include "templates/pair/tail.inc"

!!!!! Begin user code !!!!!
#undef T1
#undef T2
#undef pair

end module TypeAndContainer
!!!!! End user code !!!!!

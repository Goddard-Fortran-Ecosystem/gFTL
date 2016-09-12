program main
   use Values_mod
   use ValueTypeMap_mod
   implicit none
   class(AbstractValue),pointer :: vp
   type(ValueTypeMap) :: vtMap1,vtMap2
   integer :: k
   integer,allocatable :: k1(:)

   logical :: L
   logical,allocatable :: L1(:)

   real(kind=DP) :: r
   real(kind=DP),allocatable :: r1(:)

   character(len=MAX_LEN_ATTRIBUTE_STRING) :: s
   character(len=MAX_LEN_ATTRIBUTE_STRING),allocatable :: s1(:)

   class(AbstractValue),allocatable :: AbV

   call vtMap1%init()

   vp=>vtMap1%at('integer') 
   k= vp
   print*,k

   vp=>vtMap1%at('integer1D') 
   k1 = vp
   print *,k1

   vp=>vtMap1%at('logical')
   L = vp
   print*,L
 
   vp=>vtMap1%at('logical1D') 
   L1=vp
   print*,L1

   vp=>vtMap1%at('realDP') 
   r = vp
   print*,r
   vp=>vtMap1%at('realDP1D') 
   r1=vp
   print*,r1

   vp=>vtMap1%at('string') 
   s=vp
   print*,s


   vp=>vtMap1%at('string1D') 
   s1 = vp
   print*,s1

   ! test Copy of ValueTypeMap 
   vtMap2 = vtMap1
   vp=>vtMap2%at('string1D') 
   call vp%print() 

   L=vtMap1%get('string1D',vp)
   if(L) then
     call vp%print()
   endif

end program main

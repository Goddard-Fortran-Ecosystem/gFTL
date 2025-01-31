program main
   use Values_mod
   use AttributeMap_mod
   use Tracer_mod
   use TracerBundle_mod
   implicit none
   class(AbstractValue),pointer :: vp
   type (AttributeMap) :: aMap
   type (Tracer) :: aTracer,bTracer
   type (TracerBundle) :: aBundle
   class (Tracer),pointer :: tp


   integer :: k
   integer,allocatable :: k1d(:)

   real(kind=DP) :: r 
   real(kind=DP),allocatable :: r1d(:)

   logical :: l
   logical,allocatable :: l1d(:)

   character(len=MAX_LEN_ATTRIBUTE_STRING) :: s
   character(len=MAX_LEN_ATTRIBUTE_STRING),allocatable :: s1d(:)
   character(len=:), allocatable :: name

   character(len=MAX_LEN_KEY),allocatable :: mand(:)

   ! test init and addAttribute
   call aTracer%init('atrcer')
   call aTracer%addAttribute('r',1.0d0)
   call aTracer%addAttribute('l1d',[.true.,.true.,.false.])
   call aTracer%addAttribute('r1d',[0.5d0,5.0d0,10.0d0])

   ! test init
   call aBundle%init() 
   call aBundle%clear()

   ! test addTracer,addDefaultAttribute,addTracerAttribute
   allocate(mand(1))
   mand(1) = 'r1d'
   call aBundle%init(mand) 
   call aBundle%addTracer(aTracer)

   ! test getTracer
   L=aBundle%getTracer('atrcer',tp)
   print*,"get tracer : should be T", L

   call aBundle%addDefaultAttribute('defaultInts',[10,100,1000])
   call aBundle%addDefaultAttribute('defaultStr','hello yo')
   call aBundle%addDefaultAttribute('DStrs',['haha','hehe'])
   call aBundle%addTracerAttribute('atrcer','addon',[0.1d0,0.2d0])
      
   call aBundle%print() 

   ! test hasTracer
   L=aBundle%hasTracer('atrcer')
   print*,"has atrcer should be T:",L

   ! test get Tracer Attribute
   allocate(k1d(3))
   L=aBundle%getTracerAttribute('atrcer','defaultInts',k1d)
   if(L) print*, "k1d should be 10,100,1000:",k1d

   L=aBundle%getTracerAttribute('atrcer','defaultStr',s)
   if(L) print*, "should be 'hello yo':",s

   allocate(s1d(2))
   L=aBundle%getTracerAttribute('atrcer','DStrs',s1d)
   if(L) print*, "should be 'haha','hehe':",s1d
   

end program main

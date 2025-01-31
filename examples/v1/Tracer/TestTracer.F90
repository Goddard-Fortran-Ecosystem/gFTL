program main
   use Values_mod
   use AttributeMap_mod
   use Tracer_mod
   implicit none
   class(AbstractValue),pointer :: vp
   type (AttributeMap) :: aMap
   type (Tracer) :: aTracer,bTracer
   integer :: k
   integer,allocatable :: k1d(:)

   real(kind=DP) :: r 
   real(kind=DP),allocatable :: r1d(:)

   logical :: l
   logical,allocatable :: l1d(:)

   character(len=MAX_LEN_ATTRIBUTE_STRING) :: s
   character(len=MAX_LEN_ATTRIBUTE_STRING),allocatable :: s1d(:)
   character(len=:), allocatable :: name

   ! test init and addAttribute
   call aTracer%init('atrcer')
   call aTracer%addAttribute('r',1.0d0)
   call aTracer%addAttribute('l1d',[.true.,.true.,.false.])
   call aTracer%addAttribute('s1d',['.true.','.true.','.fals.'])

   ! test getAttribute .
  ! vp=>null()
  ! L=aTracer%getAttribute('l1d',vp)
    L=aTracer%attributes%get('l1d',vp)
    call vp%print()

   ! test setAttribute .
   allocate(l1d(3))
   l1d=[.false.,.false.,.false.]
   call aTracer%setAttribute('l1d',l1d)
   l1d=[.true.,.true.,.true.]
   L= aTracer%getAttribute('l1d',l1d)
   print*,"should be F F F :",l1d

   ! test getName
   name=aTracer%getName()
   print*,name

   ! test hasAttribute
   L= aTracer%hasAttribute('r')
   print*,"should be T :", L
   L= aTracer%hasAttribute('rr')
   print*, "should be F:", L

   ! test =
   bTracer = aTracer
   l1d=[.true.,.true.,.true.]
   L= bTracer%getAttribute('l1d',l1d)
   if(L) print*,l1d
   allocate(s1d(3))
   L= bTracer%getAttribute('s1d',s1d)
   if(L) print*,'should be .true.,.true.,.fals. :',s1d
   
   call bTracer%print()

   ! test clear()
   call bTracer%clear()

end program main

program main
   use Values_mod
   implicit none
   class(AbstractValue),pointer :: vp
   integer :: k
   integer,allocatable :: k1(:)

   logical :: L
   logical,allocatable :: L1(:)

   real(kind=DP) :: r
   real(kind=DP),allocatable :: r1(:)

   character(len=MAX_LEN_ATTRIBUTE_STRING) :: s
   character(len=MAX_LEN_ATTRIBUTE_STRING),allocatable :: s1(:)

   class(AbstractValue),allocatable :: AbV

   allocate(AbV, source=newValue(1000))
   call AbV%getValue(k)
   print*,k
   call AbV%setValue(k+1000)
   call AbV%getValue(k)
   print*,k
   deallocate(Abv)
   
   allocate(AbV, source=newValue(.false.))
   call AbV%getValue(l)
   print*,l
   call AbV%setValue(.true.)
   call AbV%getValue(l)
   print*,l

   deallocate(Abv)
   allocate(AbV,source=newValue([10.0d0,20.0d0,200.0d0]))
   allocate(r1(Abv%dims(1)))
   call AbV%getValue(r1)
   print*,r1
   r1(3)= 10000.d0
   call AbV%setValue(r1)
   call AbV%getValue(r1)
   print*,r1

  
   deallocate(Abv)
   allocate(AbV,source=newValue(['0.0d0','1.1d0','2.2d0']))
   allocate(s1(Abv%dims(1)))
   call AbV%getValue(s1)
   print*,s1
   s1= '10000.d0'
   call AbV%setValue(s1)
   call AbV%getValue(s1)
   print*,s1

   
end program main

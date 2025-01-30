program main
   use Values_mod
   use FTLAttrMap_mod, FTLAttrMap=>Map,FTLAttrMapIterator=>MapIterator
   use AttributeMap_mod
   implicit none
   class(AbstractValue),pointer :: vp
   class(AbstractValue),allocatable :: vp1
   type(AttributeMap) :: aMap, bMap
   type(FTLAttrMapIterator) :: iter
   integer :: k
   integer,allocatable :: k1d(:)

   real(kind=DP) :: r 
   real(kind=DP),allocatable :: r1d(:)

   logical :: l
   logical,allocatable :: l1d(:)

   character(len=MAX_LEN_ATTRIBUTE_STRING) :: s
   character(len=MAX_LEN_ATTRIBUTE_STRING),allocatable :: s1d(:)

   logical :: getIt

   call aMap%init()
   call aMap%insert('i',newValue(1))
   call aMap%insert('i1d',newValue([1,2]))
   call aMap%insert('l',newValue(.true.))
   call aMap%insert('l1d',newValue([.false.,.false.]))
   call aMap%insert('r',newValue(10.0d0))
   call aMap%insert('r1d',newValue([20.0d0,30.0d0]))
   call aMap%insert('s',newValue('string here'))
   call aMap%insert('s1d',newValue(['string',' there']))
   
   vp=>aMap%at('i')
   call vp%getValue(k)
   print*,'k=1: ', k  
 
   vp=>aMap%at('i1d')
   allocate(k1d(vp%dims(1)))
   call vp%getValue(K1d)
   print*,'should be [1,2]:',K1d

   vp=>aMap%at('l')
   call vp%getValue(l)
   print*,'should be T: ',l

   vp=>aMap%at('l1d')
   allocate(l1d(vp%dims(1)))
   call vp%getValue(l1d)
   print*,'should be F F : ',l1d

   vp=>aMap%at('r')
   call vp%getValue(r)
   print*,"should be 10.000: ",r

   vp=>aMap%at('r1d')
   allocate(r1d(vp%dims(1)))
   call vp%getValue(r1d)
   print*,"should be 20.0 30.0:",r1d

   vp=>aMap%at('s')
   call vp%getValue(s)
   print*,"should be string here:",s

   vp=>aMap%at('s1d')
   print*, vp%dims(1)
   allocate(s1d(vp%dims(1)))
   call vp%getValue(s1d)
   print*,"should be string there:",s1d
   deallocate(s1d)
 
   print*,"print a map"
   call aMap%print()   
   ! deepCopy
   bMap = aMap
   print*,"print b map"
   call bMap%print()   
  ! that has problem for iter%key()
   L=(bMap==aMap)
   print*,"should be test ==T : ",L

   print*,"getAttribute" 

   r1d=0.0d0
   if( aMap%getValue('r1d',r1d)) then
     print*,"r1d should be 20.0 30.0",r1d
   endif

   allocate(s1d(2))
   s1d(1)='5'
   s1d(2)='6'
   call aMap%setValue('s1d',s1d)
   vp=>aMap%at('s1d')
   print*, vp%dims(1)
   deallocate(s1d)
   allocate(s1d(vp%dims(1)))
   call vp%getValue(s1d)
   print*,"should be 5,6:",s1d
   deallocate(s1d)

  !
  !! wrong on gfortran 5.1 , right at 4.9.2 and ifort
  !

   vp=>null()
   L=aMap%get('s1d',vp)
   call vp%print()
   allocate(s1d(2))
   call vp%getValue(s1d)
   if(L)print*,"should be 5,6:",s1d
   deallocate(s1d)
   allocate(s1d(2))

   if(aMap%getValue('s1d',s1d)) then
      print*,"should be 5,6:",s1d
   end if
   
   deallocate(s1d)
   allocate(s1d(3))
   s1d=['qq','cc','dd']
   call aMap%insertValue('s3d',s1d)
   s1d=['oo','oo','oo']
   if(aMap%getValue('s3d',s1d)) then
      print*,"should be qq,cc,dd:",s1d
   end if
 
end program main

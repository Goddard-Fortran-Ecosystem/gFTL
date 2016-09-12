module TracerMap_mod
   use Tracer_mod
#define _key_string_deferred
#define _key_equal_defined
#define _Key_less_than_defined

#define _value class(Tracer)
#define _value_allocatable

#define _alt
#include "templates/map.inc"
end module TracerMap_mod

module TracerBundle_mod
   use AbstractValue_mod
   use Values_mod
   use FTLAttrMap_mod, only: AttributeMapIterator=>MapIterator
   use AttributeMap_mod
   use Tracer_mod
   use TracerMap_mod, only :TracerMap=>Map,TracerMapIterator=>MapIterator

   implicit none
   
   type :: TracerBundle
      character(len=MAX_LEN_KEY),allocatable :: mandatoryAttributes(:)
      type(AttributeMap) :: defaultValues
      type(TracerMap) :: tracers
   contains
      procedure :: initEmptyBundle
      procedure :: initBwithMan
      procedure :: initBwithManDV
      procedure :: initBwithManDVTracers

      generic :: init=>initEmptyBundle,initBwithMan,initBwithManDV,initBwithManDVTracers
      
      procedure :: addDefaultAttribute0
      procedure :: addDefaultAttribute1
      generic :: addDefaultAttribute=>addDefaultAttribute0,addDefaultAttribute1

      procedure :: addMandatoryAttribute
      procedure :: addTracer
      procedure :: getTracer
      procedure :: setTracer
      procedure :: hasTracer
      procedure :: isQualifiedTracer

      procedure :: addTracerAttribute0
      procedure :: addTracerAttribute1
      generic :: addTracerAttribute=>addTracerAttribute0,addTracerAttribute1

      procedure :: getTracerAttribute0
      procedure :: getTracerAttribute1
      generic :: getTracerAttribute=>getTracerAttribute0,getTracerAttribute1

      procedure :: setTracerAttribute0
      procedure :: setTracerAttribute1
      generic :: setTracerAttribute=>setTracerAttribute0,setTracerAttribute1

      procedure :: clearTracerBundle
      generic :: clear =>clearTracerBundle

      procedure :: printIt
      generic :: print=>printIt
   end type
contains

   subroutine initEmptyBundle(this)
      class(TracerBundle), intent(inout) :: this
      call this%defaultValues%init()
      allocate(this%mandatoryAttributes(0))
   end subroutine initEmptyBundle

   subroutine initBwithMan(this,mAttr)
      class(TracerBundle), intent(inout) :: this
      character(len=MAX_LEN_KEY),intent(in) :: mAttr(:)
      !allocate(this%mandatoryAttributes(1:size(mAttr,1)), source = mAttr)
      this%mandatoryAttributes = mAttr
      call this%defaultValues%init()
   end subroutine initBwithMan

   subroutine initBwithManDV(this,mAttr,dValues)
      class(TracerBundle), intent(inout) :: this
      character(len=MAX_LEN_KEY),intent(in) :: mAttr(:)
      type(AttributeMap),intent(in) :: dValues

      this%mandatoryAttributes = mAttr
      this%defaultValues = dValues
   end subroutine initBwithManDV

   subroutine initBwithManDVTracers(this,mAttr,dValues,tracers)
      class(TracerBundle), intent(inout) :: this
      character(len=MAX_LEN_KEY),intent(in) :: mAttr(:)
      type(AttributeMap),intent(in) :: dValues
      class(TracerMap),intent(in) :: tracers
      type(TracerMapIterator) :: iter
      integer :: i

      this%mandatoryAttributes = mAttr
      this%defaultValues = dValues
      
      iter = tracers%begin()
      do while (iter /= tracers%end())
         call this%addTracer(iter%value())
         call iter%next()
      enddo 

   end subroutine initBwithManDVTracers

   subroutine addMandatoryAttribute(this,attrName)
      class(TracerBundle),intent(inout) :: this
      character(len=*):: attrName
      integer :: n
      type(TracerMapIterator) :: iter
      type(Tracer),pointer :: tp
      character(len=MAX_LEN_KEY), allocatable :: oldValues(:)
  
      iter = this%tracers%begin()
      do while (iter /= this%tracers%end())
         tp=>iter%value()
         if(.not. tp%hasAttribute(attrName)) return        
         call iter%next()
      enddo

      n = size(this%mandatoryAttributes)
      call move_alloc(this%mandatoryAttributes,oldValues)
      allocate(this%mandatoryAttributes(n+1))
      this%mandatoryAttributes(1:n)=oldValues
      this%mandatoryAttributes(n+1) = attrName

   end subroutine addMandatoryAttribute

   subroutine addDefaultAttribute0(this,attrName,value)
      class(TracerBundle),intent(inout) :: this
      character(len=*),intent(in) :: attrName
      class(*),intent(in) :: value
      type(TracerMapIterator) :: iter
      type(Tracer),pointer :: tp

      call this%defaultValues%insertValue(attrName,value)
      
      iter = this%tracers%begin()
      do while (iter /= this%tracers%end())
         tp=>iter%value()
         if(.not. tp%hasAttribute(attrName)) then
            call tp%attributes%insertValue(attrName,value)
         endif        
         call iter%next()
         nullify(tp)
      enddo
   end subroutine addDefaultAttribute0

   subroutine addDefaultAttribute1(this,attrName,value)
      class(TracerBundle),intent(inout) :: this
      character(len=*),intent(in) :: attrName
      class(*),intent(in) :: value(:)
      type(TracerMapIterator) :: iter
      type(Tracer),pointer :: tp
      character(len=MAX_LEN_ATTRIBUTE_STRING),allocatable:: s1(:)
      integer :: n

      select type (value)
      type is (character(len=*))
         allocate(s1(size(value)))
         s1=value
         call this%defaultValues%insertValue(trim(attrName),s1)
   
         iter = this%tracers%begin()
         do while (iter /= this%tracers%end())
            tp=>iter%value()
            if(.not. tp%hasAttribute(trim(attrName))) then
               call tp%attributes%insertValue(trim(attrName),s1)
            endif        
            call iter%next()
            nullify(tp)
         enddo
         deallocate(s1)
         return
      class default
      end select
   
      call this%defaultValues%insertValue(trim(attrName),value)
   
      iter = this%tracers%begin()
      do while (iter /= this%tracers%end())
         tp=>iter%value()
         if(.not. tp%hasAttribute(trim(attrName))) then
            call tp%attributes%insertValue(trim(attrName),value)
         endif        
         call iter%next()
         nullify(tp)
      enddo

   end subroutine addDefaultAttribute1

   function isQualifiedTracer(this,trcr) result(res)
      class(TracerBundle),intent(inout) :: this
      type(Tracer),intent(in) :: trcr
      logical :: res

      type(AttributeMapIterator) :: iter
      integer :: i 
  
      ! if there is no mandatory attributes, it is qualified. 
      res = .true.
      do i = 1,size(this%mandatoryAttributes)
         res = .false.
         if(trcr%hasAttribute(trim(this%mandatoryAttributes(i)))) then
           res = .true.
           return
         endif
      enddo

   end function isQualifiedTracer

   subroutine addTracer(this,trcr)
      class(TracerBundle),intent(inout) :: this
      type(Tracer),intent(in) :: trcr
      type(AttributeMapIterator) :: iter
      type(TracerMapIterator) :: iterT
      type(Tracer),pointer :: tp
      class(Tracer),allocatable :: tmpTrcr

      if( .not. this%isQualifiedTracer(trcr)) return

      allocate(tmpTrcr,source = trcr)
      iter = this%defaultValues%begin()
      do while (iter /= this%defaultValues%end())
         if( .not. tmpTrcr%hasAttribute(iter%key())) then
            call tmpTrcr%addAttribute(iter%key(),iter%value())
         endif
         call iter%next()
      enddo
      call this%tracers%insert(tmpTrcr%name,tmpTrcr)

   end subroutine addTracer
 
   subroutine setTracer(this,trcr)
      class(TracerBundle),intent(inout) :: this
      type(Tracer),intent(in) :: trcr
      type(Tracer) :: tmpTracer
      type(AttributeMapIterator) :: iter
      integer :: i 

      if( .not. this%isQualifiedTracer(trcr)) return

      tmpTracer = trcr
      iter = this%defaultValues%begin()
      do while (iter /= this%defaultValues%end())
         if( .not. tmpTracer%hasAttribute(iter%key())) then
           call tmpTracer%addAttribute(iter%key(),iter%value())
         endif
         call iter%next()
      enddo
      call this%tracers%set(tmpTracer%name,tmpTracer)

   end subroutine setTracer

   function getTracer(this,name,trPtr) result(res)
      class(TracerBundle),intent(inout) :: this
      character(len=*),intent(in) ::name
      class(Tracer),pointer,intent(inout) :: trPtr
      logical :: res
      res= this%tracers%get(name,trPtr)
   end function getTracer

   function hasTracer(this,tName) result (has)
      class(TracerBundle),intent(in) :: this
      character(len=*),intent(in) :: tName
      logical :: has
      has = ( this%tracers%find(trim(tName)) /= this%tracers%end())
   end function

   subroutine addTracerAttribute0(this,tName,attrName,value)
      class(TracerBundle),intent(inout) :: this
      character(len=*),intent(in) :: tName
      character(len=*),intent(in) :: attrName
      class (*), intent(in) :: value
      type(TracerMapIterator) :: iter
      type(Tracer),pointer :: tp

      iter = this%tracers%find(trim(tName))

      if ( iter /= this%tracers%end()) then
         tp=>iter%value()
         call tp%attributes%insertValue(attrName,value)
      endif
   end subroutine

   subroutine addTracerAttribute1(this,tName,attrName,value)
      class(TracerBundle),intent(inout) :: this
      character(len=*),intent(in) :: tName
      character(len=*),intent(in) :: attrName
      class (*), intent(in) :: value(:)
      type(TracerMapIterator) :: iter
      type(Tracer),pointer :: tp
      character(len=MAX_LEN_ATTRIBUTE_STRING),allocatable:: s1(:)

      iter = this%tracers%find(trim(tName))
      if ( iter /= this%tracers%end()) then
         tp=>iter%value()

         ! work around for gfortran
         select type (value)
         type is (character(len=*))
            allocate(s1(size(value)))
            s1=value
            call tp%attributes%insertValue(trim(attrName),s1)
            deallocate(s1)
            return
         class default
         end select
         
         call tp%attributes%insertValue(trim(attrName),value)
      endif

   end subroutine addTracerAttribute1

   subroutine setTracerAttribute0(this,tName,attrName,value)
      class(TracerBundle),intent(inout) :: this
      character(len=*),intent(in) :: tName
      character(len=*),intent(in) :: attrName
      class (*), intent(in) :: value
      type(TracerMapIterator) :: iter
      type(Tracer),pointer :: tp

      iter = this%tracers%find(trim(tName))
      if ( iter /= this%tracers%end()) then
         tp=>iter%value()
         call tp%attributes%setValue(trim(attrName),value)
      endif

   end subroutine setTracerAttribute0

   subroutine setTracerAttribute1(this,tName,attrName,value)
      class(TracerBundle),intent(inout) :: this
      character(len=*),intent(in) :: tName
      character(len=*),intent(in) :: attrName
      class (*), intent(in) :: value(:)
      type(TracerMapIterator) :: iter
      type(Tracer),pointer :: tp
      character(len=MAX_LEN_ATTRIBUTE_STRING),allocatable:: s1(:)

      iter = this%tracers%find(trim(tName))
      if ( iter /= this%tracers%end()) then
         tp=>iter%value()
         ! work around for gfortran
         select type (value)
         type is (character(len=*))
            allocate(s1(size(value)))
            s1=value
            call tp%attributes%setValue(trim(attrName),s1)
            deallocate(s1)
            return
         class default
         end select

         call tp%attributes%setValue(trim(attrName),value)
      endif
   end subroutine setTracerAttribute1

   function getTracerAttribute0(this,tName,attrName,value) result(res)
      class(TracerBundle),intent(inout) :: this
      character(len=*),intent(in) :: tName
      character(len=*),intent(in) :: attrName
      class (*), intent(inout) :: value
      type(TracerMapIterator) :: iter
      type(Tracer),pointer :: tp
      logical:: res

      res = .false.

      iter = this%tracers%find(trim(tName))

      if ( iter /= this%tracers%end()) then
         tp=>iter%value()
         res = tp%attributes%getValue(trim(attrName),value)
      endif
   end function getTracerAttribute0

   function getTracerAttribute1(this,tName,attrName,value) result(res)
      class(TracerBundle),intent(inout) :: this
      character(len=*),intent(in) :: tName
      character(len=*),intent(in) :: attrName
      class (*), intent(inout) :: value(:)
      type(TracerMapIterator) :: iter
      type(Tracer),pointer :: tp
      logical:: res
      character(len=MAX_LEN_ATTRIBUTE_STRING),allocatable:: s1(:)

      res = .false.

      iter = this%tracers%find(trim(tName))
      if ( iter /= this%tracers%end()) then
         tp=>iter%value()
         ! work around for gfortran
         select type (value)
         type is (character(len=*))
            allocate(s1(size(value)))
            res= tp%attributes%getValue(trim(attrName),s1)
            value=s1
            deallocate(s1)
            return
         class default
         end select
         res = tp%attributes%getValue(trim(attrName),value)
      endif
   end function getTracerAttribute1

   subroutine clearTracerBundle(this)

      class(TracerBundle),intent(inout) :: this
      
      if(this%tracers%size() /=0) call this%tracers%clear()
      if(this%defaultValues%size() /=0) call this%defaultValues%clear()
      deallocate(this%mandatoryAttributes)

   end subroutine

   subroutine printIt(this)

      class(TracerBundle),intent(inout) :: this
      type(TracerMapIterator) :: iter
      type(Tracer),pointer :: p
      print*, "default values: "
      call this%defaultValues%print()
      print*, "loop tracers: "
      iter = this%tracers%begin()
      do while (iter /= this%tracers%end())
         p=>iter%value()
         call p%print()
         call iter%next()
      end do

   end subroutine printIt

end module TracerBundle_mod

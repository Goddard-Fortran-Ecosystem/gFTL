module FTLAttrMap_mod
   use AbstractValue_mod
#define _key_string_deferred
#define _key_equal_defined
#define _Key_less_than_defined

#define _value class(AbstractValue)
#define _value_allocatable
#define _alt
#include "templates/map.inc"

end module FTLAttrMap_mod

module AttributeMap_mod
   use Values_mod
   use FTLAttrMap_mod, FTLAttrMap=>Map,FTLAttrMapIterator=>MapIterator
   implicit none
   private

   public :: AttributeMap

   type,extends(FTLAttrMap) :: AttributeMap
      ! this Value Type map is for IO purpose
      type(ValueTypeMap) :: vp
   contains
      procedure :: initAttrMap
      procedure :: writeUnformatted
      procedure :: readUnformatted
      procedure :: equals
      generic :: operator(==) =>equals

      procedure :: insertAttribute0
      procedure :: insertAttribute1
      generic :: init=>initAttrMap
      generic :: insertValue=>insertAttribute0,insertAttribute1

      procedure :: setAttribute0
      procedure :: setAttribute1
      generic :: setValue=>setAttribute0,setAttribute1
     
      procedure :: getAttribute0
      procedure :: getAttribute1
      generic :: getValue=>getAttribute0,getAttribute1
 
      procedure :: copyAP
      generic :: assignment(=)=>copyAP

      procedure :: printIt
      generic :: print=>printIt
   end type

contains

  subroutine initAttrMap(this)
     class(AttributeMap),intent(inout) :: this
     call this%vp%init()
  end subroutine initAttrMap
  
  subroutine insertAttribute0(this,name,value)
     class(AttributeMap),intent(inout) :: this
     character(len=*) :: name
     class(*),intent(in) :: value

     select type(value)
     class is (AbstractValue)
        call this%insert(trim(name),value)
     type is (integer)
        call this%insert(trim(name),newValue(value))
     type is ( logical )
        call this%insert(trim(name),newValue(value))
     type is ( real(KIND=DP))
        call this%insert(trim(name),newValue(value))
     type is (character(len=*))
        call this%insert(trim(name),newValue(value))
     class default
        print*, "wrong attribute type insertAttribute0"
     end select
  end subroutine insertAttribute0

  subroutine insertAttribute1(this,name,value)
     class(AttributeMap),intent(inout) :: this
     character(len=*) :: name
     class(*),intent(in) :: value(:)
     
     select type(value)
     type is (integer)
        call this%insert(trim(name),newValue(value))
     type is ( logical )
        call this%insert(trim(name),newValue(value))
     type is ( real(KIND=DP))
        call this%insert(trim(name),newValue(value))
     type is (character(len=*))
        call this%insert(trim(name),newValue(value))
     class default
        print*, "wrong attribute type insertAttribute1"
     end select
  end subroutine insertAttribute1

  function getAttribute0(this,name,value) result(res)
     class(AttributeMap),intent(inout) :: this
     character(len=*) :: name
     class(*),intent(inout) :: value
     class(AbstractValue),pointer :: AbV
     logical :: res

     res = .false.
     if(this%get(trim(name),Abv)) then
        call AbV%getValue(value)
        res = .true.
     endif

  end function getAttribute0

  function getAttribute1(this,name,value) result(res)
     class(AttributeMap),intent(inout) :: this
     character(len=*) :: name
     class(*),intent(inout) :: value(:)
     class(AbstractValue),pointer :: AbV
     character(len=MAX_LEN_ATTRIBUTE_STRING),allocatable:: s1(:)
     integer :: n
     logical :: res

     res = .false.

     if(this%get(trim(name),Abv)) then
        select type(value)
        type is (character(len=*))
     !
     !this is a work around for gfortran. 
     !
           n=size(value)
           allocate(s1(n))
           call AbV%getValue(s1)
           value = s1
           deallocate(s1)
        class default
           call AbV%getValue(value)
        end select
        res = .true.
     endif
  end function getAttribute1

  subroutine setAttribute0(this,name,value)
     class(AttributeMap),intent(inout) :: this
     character(len=*) :: name
     class(*),intent(in) :: value

     select type(value)
     class is (AbstractValue)
        call this%set(trim(name),value)
     type is (integer)
        call this%set(trim(name),newValue(value))
     type is ( logical )
        call this%set(trim(name),newValue(value))
     type is ( real(KIND=DP))
        call this%set(trim(name),newValue(value))
     type is (character(len=*))
        call this%set(trim(name),newValue(value))
     class default
        print*, "wrong attribute type set0"
     endselect

  end subroutine setAttribute0

  subroutine setAttribute1(this,name,value)
     class(AttributeMap),intent(inout) :: this
     character(len=*) :: name
     class(*),intent(in) :: value(:)
     
     select type(value)
     type is (integer)
        call this%set(trim(name),newValue(value))
     type is ( logical )
        call this%set(trim(name),newValue(value))
     type is ( real(KIND=DP))
        call this%set(trim(name),newValue(value))
     type is (character(len=*))
        call this%set(trim(name),newValue(value))
     class default
        print*, "wrong attribute type set 1"
     endselect

  end subroutine setAttribute1

  subroutine writeUnformatted(this, unit)
     use ValueTypeMap_mod, FTLValueTypeIterator=>MapIterator
     use FTLAttrMap_mod,FTLAttrMapIterator=>MapIterator
     class (AttributeMap), intent(in) :: this
     integer, intent(in) :: unit

     type (FTLAttrMapIterator) :: iter
     type (FTLValueTypeIterator) :: iterb
     class (AbstractValue), pointer :: p1,p2

     write(unit) this%size()
     iter = this%begin()
     do while (iter /= this%end())

        write(unit) iter%key()
        p1 => iter%value()
        write(unit) p1%name
        !iterb=this%vp%find(trim(p1%name))
        !p2=> iterb%value()

       ! p2=>this%vp%at(trim(p1%name))
       ! call p2%writeUnformatted(unit)

        call iter%next()
    end do
   end subroutine writeUnformatted

   subroutine readUnformatted(this, unit)
    class (AttributeMap),intent(inout) :: this
    integer, intent(in) :: unit

    integer :: n
    integer :: i
    class (AbstractValue), pointer :: p1,p2,q
    character(len=MAX_LEN_KEY) :: key,name

    read(unit) n
    do i = 1, n
      read(unit) key
      read(unit) name
      p1 => this%at(trim(key))
      p2 => this%vp%at(trim(p1%name))
      q =>p2%readUnformatted(unit)
      call this%insert(trim(key), q)
    end do

  end subroutine readUnformatted

  subroutine copyAP(to,from)
     class(AttributeMap),intent(inout) :: to
     class(AttributeMap),intent(in) :: from
     call to%deepCopy(from)
     to%vp = from%vp
  end subroutine copyAP

  logical function equals(this, b)
    class (AttributeMap), intent(in) :: this
    type (AttributeMap), intent(in) :: b

    type (FTLAttrMapIterator) :: iter
    type (FTLAttrMapIterator) :: iterb
    character(LEN=MAX_LEN_KEY),pointer :: sp
    class (AbstractValue), pointer :: p1
    class (AbstractValue), pointer :: p2


    equals = .true.
    if (this%size() /= b%size()) then
      equals = .false.
      print*,'different size',this%size(), b%size()
      return
    end if

    iter = this%begin()
    
    do while (iter /= this%end())
      if ( b%find(iter%key()) == b%end()) then
        equals = .false.
        print*,'different key'
        return
      end if

      p1 => iter%value()
      iterb=b%find(iter%key())
      p2 => iterb%value()

      if (.not. (p1%equals(p2))) then
        equals = .false.
        print*,'different value for key <',trim(iter%key()),'>'
        call p1%print()
        call p2%print()
        return
      end if

      call iter%next()
    end do

  end function equals

  subroutine printIt(this)
    use FTLAttrMap_mod,FTLAttrMapIterator=>MapIterator
    class (AttributeMap), intent(in) :: this
    type (FTLAttrMapIterator) :: iter
    class (AbstractValue), pointer :: p

    iter = this%FTLAttrMap%begin()

    do while( iter /= this%end())
      print*,"Attribute Name: ", iter%key()
      p=>iter%value()  
      call p%print() 
      call iter%next() 
    end do
 
  end subroutine printIt

end module AttributeMap_mod

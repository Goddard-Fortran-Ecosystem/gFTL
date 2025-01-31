module integerValue_mod
  use AbstractValue_mod
  implicit none
  private

#define TYPE integerValue

  public :: TYPE
  public :: newValue
  public :: toType
  public :: toPointer
  public :: assignment(=)

  type, extends(AbstractValue) :: TYPE
    integer :: value
  contains   
    procedure :: equals
    procedure :: clear
    procedure :: print => printIt ! gfortran workaround
    procedure :: writeUnformatted
    procedure :: readUnformatted
    procedure :: toString
    procedure :: getScalar
    procedure :: get1DValue
    procedure :: setScalar
    procedure :: set1DValue

  end type TYPE

  interface newValue
    module procedure constructor
  end interface

  interface assignment(=)
     module procedure toType_
  end interface

  interface toType
     module procedure toType_
  end interface

  interface toPointer
     module procedure toPointerType
  end interface toPointer

contains

  function constructor(value) result(entry)
    type (TYPE) :: entry
    integer, intent(in) :: value 
    
    entry%value = value
    entry%rank = 0
    entry%name = "integer"
    allocate(entry%dims(0))

  end function constructor

  subroutine toType_(value, entry)
    integer , intent(inout) :: value 
    class (AbstractValue), intent(in) :: entry

    integer :: i

    select type (entry)
    type is (TYPE)
        value = entry%value
    class default
      call stop_model('Illegal conversion of integerValue.',255)
    end select
  end subroutine toType_

  function toPointerType(entry, cast) result(ptr)
    integer, pointer :: ptr 
    class (AbstractValue), target, intent(in) :: entry
    integer :: cast 

    select type (q => entry)
    type is (integerValue)
       ptr => q%value
    class default
      call stop_model('Illegal association of integerValue.',255)
    end select
  end function toPointerType

  subroutine getScalar(this, value)
     class(TYPE),intent(in) :: this
     class(*),intent(inout) :: value
     select type (value)
     type is (integer)
        value=this%value
     class default
        call stop_model('should be integer type of value.',255)
     end select
  end subroutine getScalar

  subroutine get1DValue(this, value)
     class(TYPE),intent(in) :: this
     class(*),intent(inout) :: value(:)
     !class(*),intent(out),allocatable :: value(:)
     call stop_model('value should not be 1D integer',255)
  end subroutine get1DValue

  subroutine setScalar(this, value)
     class(TYPE),intent(inout) :: this
     class(*),intent(in) :: value
     select type (value)
     type is (integer)
        this%value=value
     class default
        call stop_model('should be integer type of value.',255)
     end select
  end subroutine setScalar

  subroutine set1DValue(this, value)
     class(TYPE),intent(inout) :: this
     class(*),intent(in) :: value(:)
     call stop_model('value should not be 1D integer',255)
  end subroutine set1DValue

  logical function equals(this, b)
    class (integerValue), intent(in) :: this
    class (AbstractValue), intent(in) :: b

    select type (p => b)
    class is (integerValue)
      if ((this%value == p%value)) then
        equals = .true.
      else
        equals = .false.
      end if
    class default
      equals = .false.
    end select

  end function equals

  subroutine printIt(this)
    class (integerValue), intent(in) :: this
    print*,'  Type:  ', 'integerValue'
    print*,'  Value: <', this%value,'>'
    print*,'--------------'
  end subroutine printIt

  function toString(this) result(string)
    use StringUtilities_mod, only: toStringElemental => toString
    class (integerValue), intent(in) :: this
    character(len=MAX_LEN_LINE) :: string

    string = toStringElemental(this%value)

  contains

    function join(strArray, separator) result(string)
      character(len=*), intent(in) :: strArray(:)
      character(len=*), intent(in) :: separator
      character(len=MAX_LEN_LINE) :: string

      integer :: i
      string = trim(strArray(1))
      do i = 2, size(strArray)
        string = trim(string) // trim(separator) // trim(strArray(i))
      end do
    end function join

  end function toString

  subroutine writeUnformatted(this, unit)
    class (integerValue), intent(in) :: this
    integer, intent(in) :: unit

    
    
    write(unit) this%value

  end subroutine writeUnformatted

  function readUnformatted(this, unit) result(new)
    
    class (integerValue), intent(in) :: this
    integer, intent(in) :: unit
    class (AbstractValue), pointer :: new

    integer :: rank
    integer, pointer :: value 
    

    allocate(value )
    

    read(unit) value

    

    allocate(new, source=newValue(value))
    deallocate(value)
    

  end function readUnformatted

  subroutine clear(this)
    class (integerValue), intent(inout) :: this
    
  end subroutine clear

end module integerValue_mod
#undef TYPE


module integer1dValue_mod
  use AbstractValue_mod
  use integerValue_mod
  implicit none
  private

#define TYPE integer1dValue

  public :: TYPE
  public :: newValue
  public :: toType
  public :: toPointer
  public :: assignment(=)

  type, extends(AbstractValue) :: TYPE
    integer, allocatable :: value (:)

  contains   
    procedure :: equals
    procedure :: clear
    procedure :: print => printIt ! gfortran workaround
    procedure :: writeUnformatted
    procedure :: readUnformatted
    procedure :: toString
    procedure :: getScalar
    procedure :: get1DValue
    procedure :: setScalar
    procedure :: set1DValue
  end type TYPE

  interface newValue
    module procedure constructor
  end interface

  interface assignment(=)
     module procedure toType_
  end interface

  interface toType
     module procedure toType_
  end interface

  interface toPointer
     module procedure toPointerType
  end interface toPointer

contains

  function constructor(value) result(entry)
    type (TYPE) :: entry
    integer, intent(in) :: value (:)
 
    allocate(entry%value (size(value,1)))

    entry%value = value
    entry%rank = 1
    entry%name = "integer1D"
    allocate(entry%dims(1))
    entry%dims(1)=size(value,1)

  end function constructor

  subroutine toType_(value, entry)
     integer , allocatable, intent(inout) :: value (:)
    class (AbstractValue), intent(in) :: entry

    integer :: i

    select type (entry)
    type is (TYPE)

#ifdef __GFORTRAN__
        value = entry%value
#else
        allocate(value, source=entry%value)
#endif

    class default
      call stop_model('Illegal conversion of integer1dValue.',255)
    end select
  end subroutine toType_

  function toPointerType(entry, cast) result(ptr)
    integer, pointer :: ptr (:)
    class (AbstractValue), target, intent(in) :: entry
    integer :: cast (:)

    select type (q => entry)
    type is (integer1dValue)
       ptr => q%value
    class default
      call stop_model('Illegal association of integer1dValue.',255)
       print*,'Illegal association of integer1dValue.'
    end select
  end function toPointerType

  subroutine getScalar(this, value)
     class(TYPE),intent(in) :: this
     class(*),intent(inout) :: value
     call stop_model('value should not be integer scalar',255)
  end subroutine getScalar

  subroutine get1DValue(this, value)
     class(TYPE),intent(in) :: this
     class(*),intent(inout) :: value(:)

     select type (value)
     type is (integer)
        value=this%value
     class default
        call stop_model('should be 1D integer type of value.',255)
     end select
  end subroutine get1DValue

  subroutine setScalar(this, value)
     class(TYPE),intent(inout) :: this
     class(*),intent(in) :: value
     call stop_model('value should not be integer scalar',255)
  end subroutine setScalar

  subroutine set1DValue(this, value)
     class(TYPE),intent(inout) :: this
     class(*),intent(in) :: value(:)
     
     if(this%dims(1) /= size(value,1)) then
        call stop_model('should not change dimension through setValue call')
     endif

     select type (value)
     type is (integer)
        this%value=value
     class default
        call stop_model('should be 1D integer type of value.',255)
     end select
  end subroutine set1DValue

  logical function equals(this, b)
    class (integer1dValue), intent(in) :: this
    class (AbstractValue), intent(in) :: b

    select type (p => b)
    class is (integer1dValue)
      if (all(this%value == p%value)) then
        equals = .true.
      else
        equals = .false.
      end if
    class default
      equals = .false.
    end select

  end function equals

  subroutine printIt(this)
    class (integer1dValue), intent(in) :: this
    print*,'  Type:  ', 'integer1dValue'
    print*,'  Value: <', this%value,'>'
    print*,'--------------'
  end subroutine printIt

  function toString(this) result(string)
    use StringUtilities_mod, only: toStringElemental => toString
    class (integer1dValue), intent(in) :: this
    character(len=MAX_LEN_LINE) :: string

    string = join(reshape(toStringElemental(this%value),(/size(this%value)/)),', ')

  contains

    function join(strArray, separator) result(string)
      character(len=*), intent(in) :: strArray(:)
      character(len=*), intent(in) :: separator
      character(len=MAX_LEN_LINE) :: string

      integer :: i
      string = trim(strArray(1))
      do i = 2, size(strArray)
        string = trim(string) // trim(separator) // trim(strArray(i))
      end do
    end function join

  end function toString

  subroutine writeUnformatted(this, unit)
    class (integer1dValue), intent(in) :: this
    integer, intent(in) :: unit

    write(unit) shape(this%value)
    
    write(unit) this%value

  end subroutine writeUnformatted

  function readUnformatted(this, unit) result(new)
    
    class (integer1dValue), intent(in) :: this
    integer, intent(in) :: unit
    class (AbstractValue), pointer :: new

    integer :: rank
    integer, pointer :: value (:)
    
    integer :: attributeShape(1)

    read(unit) attributeShape
    allocate(value(attributeShape(1)))
    
    

    read(unit) value

    

    allocate(new, source=newValue(value))
    deallocate(value)
    

  end function readUnformatted

  subroutine clear(this)
    class (integer1dValue), intent(inout) :: this
    deallocate(this%value)
  end subroutine clear

end module integer1dValue_mod
#undef TYPE


module logicalValue_mod
  use AbstractValue_mod
  
  implicit none
  private

#define TYPE logicalValue

  public :: TYPE
  public :: newValue
  public :: toType
  public :: toPointer
  public :: assignment(=)

  type, extends(AbstractValue) :: TYPE
    logical :: value
  contains   
    procedure :: equals
    procedure :: clear
    procedure :: print => printIt ! gfortran workaround
    procedure :: writeUnformatted
    procedure :: readUnformatted
    procedure :: toString
    procedure :: getScalar
    procedure :: get1DValue
    procedure :: setScalar
    procedure :: set1DValue
  end type TYPE

  interface newValue
    module procedure constructor
  end interface

  interface assignment(=)
     module procedure toType_
  end interface

  interface toType
     module procedure toType_
  end interface

  interface toPointer
     module procedure toPointerType
  end interface toPointer

contains

  function constructor(value) result(entry)
    type (TYPE) :: entry
    logical, intent(in) :: value 
    

    entry%value = value
    entry%rank = 0
    entry%name = "logical"
    allocate(entry%dims(0))

  end function constructor

  subroutine toType_(value, entry)
     logical , intent(inout) :: value 
    class (AbstractValue), intent(in) :: entry

    integer :: i

    select type (entry)
    type is (TYPE)
        value = entry%value



    class default
      call stop_model('Illegal conversion of logicalValue.',255)
    end select
  end subroutine toType_

  function toPointerType(entry, cast) result(ptr)
    logical, pointer :: ptr 
    class (AbstractValue), target, intent(in) :: entry
    logical :: cast 

    select type (q => entry)
    type is (logicalValue)
       ptr => q%value
    class default
      call stop_model('Illegal association of logicalValue.',255)
    end select
  end function toPointerType

  subroutine getScalar(this, value)
     class(TYPE),intent(in) :: this
     class(*),intent(inout) :: value
     select type (value)
     type is (logical)
        value=this%value
     class default
        call stop_model('shaould be logical type of value.',255)
     end select
  end subroutine getScalar

  subroutine get1DValue(this, value)
     class(TYPE),intent(in) :: this
     class(*),intent(inout) :: value(:)
     call stop_model('value should not be 1D logical',255)
  end subroutine get1DValue

  subroutine setScalar(this, value)
     class(TYPE),intent(inout) :: this
     class(*),intent(in) :: value
     select type (value)
     type is (logical)
        this%value=value
     class default
        call stop_model('shaould be logical type of value.',255)
     end select
  end subroutine setScalar

  subroutine set1DValue(this, value)
     class(TYPE),intent(inout) :: this
     class(*),intent(in) :: value(:)
     call stop_model('value should not be 1D logical',255)
  end subroutine set1DValue

  logical function equals(this, b)
    class (logicalValue), intent(in) :: this
    class (AbstractValue), intent(in) :: b

    select type (p => b)
    class is (logicalValue)
      if ((this%value .eqv. p%value)) then
        equals = .true.
      else
        equals = .false.
      end if
    class default
      equals = .false.
    end select

  end function equals

  subroutine printIt(this)
    class (logicalValue), intent(in) :: this
    print*,'  Type:  ', 'logicalValue'
    print*,'  Value: <', this%value,'>'
    print*,'--------------'
  end subroutine printIt

  function toString(this) result(string)
    use StringUtilities_mod, only: toStringElemental => toString
    class (logicalValue), intent(in) :: this
    character(len=MAX_LEN_LINE) :: string

    string = toStringElemental(this%value)

  contains

    function join(strArray, separator) result(string)
      character(len=*), intent(in) :: strArray(:)
      character(len=*), intent(in) :: separator
      character(len=MAX_LEN_LINE) :: string

      integer :: i
      string = trim(strArray(1))
      do i = 2, size(strArray)
        string = trim(string) // trim(separator) // trim(strArray(i))
      end do
    end function join

  end function toString

  subroutine writeUnformatted(this, unit)
    class (logicalValue), intent(in) :: this
    integer, intent(in) :: unit

    
    
    write(unit) this%value

  end subroutine writeUnformatted

  function readUnformatted(this, unit) result(new)
    
    class (logicalValue), intent(in) :: this
    integer, intent(in) :: unit
    class (AbstractValue), pointer :: new

    integer :: rank
    logical, pointer :: value 
    

    allocate(value )
    

    read(unit) value

    

    allocate(new, source=newValue(value))
    deallocate(value)
    

  end function readUnformatted

  subroutine clear(this)
    class (logicalValue), intent(inout) :: this
    
  end subroutine clear

end module logicalValue_mod
#undef TYPE


module logical1dValue_mod
  use AbstractValue_mod
  use logicalValue_mod
  implicit none
  private

#define TYPE logical1dValue

  public :: TYPE
  public :: newValue
  public :: toType
  public :: toPointer
  public :: assignment(=)

  type, extends(AbstractValue) :: TYPE
    logical, allocatable :: value (:)

  contains   
    procedure :: equals
    procedure :: clear
    procedure :: print => printIt ! gfortran workaround
    procedure :: writeUnformatted
    procedure :: readUnformatted
    procedure :: toString
    procedure :: getScalar
    procedure :: get1DValue
    procedure :: setScalar
    procedure :: set1DValue
  end type TYPE

  interface newValue
    module procedure constructor
  end interface

  interface assignment(=)
     module procedure toType_
  end interface

  interface toType
     module procedure toType_
  end interface

  interface toPointer
     module procedure toPointerType
  end interface toPointer

contains

  function constructor(value) result(entry)
    type (TYPE) :: entry
    logical, intent(in) :: value (:)
    allocate(entry%value (size(value,1)))

    entry%value = value
    entry%rank = 1
    entry%name = "logical1D"
    allocate(entry%dims(1))
    entry%dims(1) = size(value,1)

  end function constructor

  subroutine toType_(value, entry)
     logical , allocatable, intent(inout) :: value (:)
    class (AbstractValue), intent(in) :: entry

    integer :: i

    select type (entry)
    type is (TYPE)

#ifdef __GFORTRAN__
        value = entry%value
#else
        allocate(value, source=entry%value)
#endif

    class default
      call stop_model('Illegal conversion of logical1dValue.',255)
    end select
  end subroutine toType_

  function toPointerType(entry, cast) result(ptr)
    logical, pointer :: ptr (:)
    class (AbstractValue), target, intent(in) :: entry
    logical :: cast (:)

    select type (q => entry)
    type is (logical1dValue)
       ptr => q%value
    class default
      call stop_model('Illegal association of logical1dValue.',255)
      print*,'Illegal association of logical1dValue.'
    end select
  end function toPointerType

  subroutine getScalar(this, value)
     class(TYPE),intent(in) :: this
     class(*),intent(inout) :: value
     call stop_model('value should not be logical scalar',255)
  end subroutine getScalar

  subroutine get1DValue(this, value)
     class(TYPE),intent(in) :: this
     class(*),intent(inout) :: value(:)

     select type (value)
     type is (logical)
        value=this%value
     class default
        call stop_model('should be 1D logical type of value.',255)
     end select
  end subroutine get1DValue

  subroutine setScalar(this, value)
     class(TYPE),intent(inout) :: this
     class(*),intent(in) :: value
     call stop_model('value should not be logical scalar',255)
  end subroutine setScalar

  subroutine set1DValue(this, value)
     class(TYPE),intent(inout) :: this
     class(*),intent(in) :: value(:)

     if(this%dims(1) /= size(value,1)) then
        call stop_model('should not change dimension through setValue call')
     endif
     select type (value)
     type is (logical)
        this%value = value
     class default
        call stop_model('should be 1D logical type of value.',255)
     end select
  end subroutine set1DValue


  logical function equals(this, b)
    class (logical1dValue), intent(in) :: this
    class (AbstractValue), intent(in) :: b

    select type (p => b)
    class is (logical1dValue)
      if (all(this%value .eqv. p%value)) then
        equals = .true.
      else
        equals = .false.
      end if
    class default
      equals = .false.
    end select

  end function equals

  subroutine printIt(this)
    class (logical1dValue), intent(in) :: this
    print*,'  Type:  ', 'logical1dValue'
    print*,'  Value: <', this%value,'>'
    print*,'--------------'
  end subroutine printIt

  function toString(this) result(string)
    use StringUtilities_mod, only: toStringElemental => toString
    class (logical1dValue), intent(in) :: this
    character(len=MAX_LEN_LINE) :: string

    string = join(reshape(toStringElemental(this%value),(/size(this%value)/)),', ')

  contains

    function join(strArray, separator) result(string)
      character(len=*), intent(in) :: strArray(:)
      character(len=*), intent(in) :: separator
      character(len=MAX_LEN_LINE) :: string

      integer :: i
      string = trim(strArray(1))
      do i = 2, size(strArray)
        string = trim(string) // trim(separator) // trim(strArray(i))
      end do
    end function join

  end function toString

  subroutine writeUnformatted(this, unit)
    class (logical1dValue), intent(in) :: this
    integer, intent(in) :: unit

    write(unit) shape(this%value)
    
    write(unit) this%value

  end subroutine writeUnformatted

  function readUnformatted(this, unit) result(new)
    
    class (logical1dValue), intent(in) :: this
    integer, intent(in) :: unit
    class (AbstractValue), pointer :: new

    integer :: rank
    logical, pointer :: value (:)
    

    integer :: attributeShape(1)

    read(unit) attributeShape
    allocate(value(attributeShape(1)))
    
    

    read(unit) value

    

    allocate(new, source=newValue(value))
    deallocate(value)
    

  end function readUnformatted

  subroutine clear(this)
    class (logical1dValue), intent(inout) :: this
    deallocate(this%value)
  end subroutine clear

end module logical1dValue_mod
#undef TYPE


module RealDPValue_mod
  use AbstractValue_mod
  
  implicit none
  private

#define TYPE RealDPValue

  public :: TYPE
  public :: newValue
  public :: toType
  public :: toPointer
  public :: assignment(=)

  type, extends(AbstractValue) :: TYPE
    real(kind=DP) :: value
  contains   
    procedure :: equals
    procedure :: clear
    procedure :: print => printIt ! gfortran workaround
    procedure :: writeUnformatted
    procedure :: readUnformatted
    procedure :: toString
    procedure :: getScalar
    procedure :: get1DValue
    procedure :: setScalar
    procedure :: set1DValue
  end type TYPE

  interface newValue
    module procedure constructor
  end interface

  interface assignment(=)
     module procedure toType_
  end interface

  interface toType
     module procedure toType_
  end interface

  interface toPointer
     module procedure toPointerType
  end interface toPointer

contains

  function constructor(value) result(entry)
    type (TYPE) :: entry
    real(kind=DP), intent(in) :: value 

    entry%value = value
    entry%rank = 0
    entry%name = "realDP"
    allocate(entry%dims(0))

  end function constructor

  subroutine toType_(value, entry)
     real(kind=DP) , intent(inout) :: value 
    class (AbstractValue), intent(in) :: entry

    integer :: i

    select type (entry)
    type is (TYPE)
        value = entry%value



    class default
      call stop_model('Illegal conversion of RealDPValue.',255)
    end select
  end subroutine toType_

  function toPointerType(entry, cast) result(ptr)
    real(kind=DP), pointer :: ptr 
    class (AbstractValue), target, intent(in) :: entry
    real(kind=DP) :: cast 

    select type (q => entry)
    type is (RealDPValue)
       ptr => q%value
    class default
      call stop_model('Illegal association of RealDPValue.',255)
      print*,'Illegal association of RealDPValue.'
    end select
  end function toPointerType

  subroutine getScalar(this, value)
     class(TYPE),intent(in) :: this
     class(*),intent(inout) :: value
     select type (value)
     type is (real(kind=DP))
        value=this%value
     class default
        call stop_model('should be real dp type ',255)
     end select
  end subroutine getScalar

  subroutine get1DValue(this, value)
     class(TYPE),intent(in) :: this
     class(*),intent(inout) :: value(:)
     call stop_model('value should not be 1D real',255)
  end subroutine get1DValue

  subroutine setScalar(this, value)
     class(TYPE),intent(inout) :: this
     class(*),intent(in) :: value
     select type (value)
     type is (real(kind=DP))
        this%value=value
     class default
        call stop_model('should be real dp type ',255)
     end select
  end subroutine setScalar

  subroutine set1DValue(this, value)
     class(TYPE),intent(inout) :: this
     class(*),intent(in) :: value(:)
     call stop_model('value should not be 1D real',255)
  end subroutine set1DValue

  logical function equals(this, b)
    class (RealDPValue), intent(in) :: this
    class (AbstractValue), intent(in) :: b

    select type (p => b)
    class is (RealDPValue)
      if ((this%value == p%value)) then
        equals = .true.
      else
        equals = .false.
      end if
    class default
      equals = .false.
    end select

  end function equals

  subroutine printIt(this)
    class (RealDPValue), intent(in) :: this
    print*,'  Type:  ', 'RealDPValue'
    print*,'  Value: <', this%value,'>'
    print*,'--------------'
  end subroutine printIt

  function toString(this) result(string)
    use StringUtilities_mod, only: toStringElemental => toString
    class (RealDPValue), intent(in) :: this
    character(len=MAX_LEN_LINE) :: string

    string = toStringElemental(this%value)

  contains

    function join(strArray, separator) result(string)
      character(len=*), intent(in) :: strArray(:)
      character(len=*), intent(in) :: separator
      character(len=MAX_LEN_LINE) :: string

      integer :: i
      string = trim(strArray(1))
      do i = 2, size(strArray)
        string = trim(string) // trim(separator) // trim(strArray(i))
      end do
    end function join

  end function toString

  subroutine writeUnformatted(this, unit)
    class (RealDPValue), intent(in) :: this
    integer, intent(in) :: unit

    
    
    write(unit) this%value

  end subroutine writeUnformatted

  function readUnformatted(this, unit) result(new)
    
    class (RealDPValue), intent(in) :: this
    integer, intent(in) :: unit
    class (AbstractValue), pointer :: new

    integer :: rank
    real(kind=DP), pointer :: value 
    

    allocate(value )
    

    read(unit) value

    

    allocate(new, source=newValue(value))
    deallocate(value)
    

  end function readUnformatted

  subroutine clear(this)
    class (RealDPValue), intent(inout) :: this
    
  end subroutine clear

end module RealDPValue_mod
#undef TYPE


module RealDP1dValue_mod
  use AbstractValue_mod
  use RealDPValue_mod
  implicit none
  private

#define TYPE RealDP1dValue

  public :: TYPE
  public :: newValue
  public :: toType
  public :: toPointer
  public :: assignment(=)

  type, extends(AbstractValue) :: TYPE
    real(kind=DP), allocatable :: value (:)

  contains   
    procedure :: equals
    procedure :: clear
    procedure :: print => printIt ! gfortran workaround
    procedure :: writeUnformatted
    procedure :: readUnformatted
    procedure :: toString
    procedure :: getScalar
    procedure :: get1DValue
    procedure :: setScalar
    procedure :: set1DValue
  end type TYPE

  interface newValue
    module procedure constructor
  end interface

  interface assignment(=)
     module procedure toType_
  end interface

  interface toType
     module procedure toType_
  end interface

  interface toPointer
     module procedure toPointerType
  end interface toPointer

contains

  function constructor(value) result(entry)
    type (TYPE) :: entry
    real(kind=DP), intent(in) :: value (:)
    allocate(entry%value (size(value,1)))

    entry%value = value
    entry%rank = 1
    entry%name = "realDP1D"
    allocate(entry%dims(1))
    entry%dims(1)= size(value,1)

  end function constructor

  subroutine toType_(value, entry)
     real(kind=DP) , allocatable, intent(inout) :: value (:)
    class (AbstractValue), intent(in) :: entry

    integer :: i

    select type (entry)
    type is (TYPE)

#ifdef __GFORTRAN__
        value = entry%value
#else
        allocate(value, source=entry%value)
#endif

    class default
      call stop_model('Illegal conversion of RealDP1dValue.',255)
    end select
  end subroutine toType_

  function toPointerType(entry, cast) result(ptr)
    real(kind=DP), pointer :: ptr (:)
    class (AbstractValue), target, intent(in) :: entry
    real(kind=DP) :: cast (:)

    select type (q => entry)
    type is (RealDP1dValue)
       ptr => q%value
    class default
      call stop_model('Illegal association of RealDP1dValue.',255)
      print*,'Illegal association of RealDP1dValue.'
    end select
  end function toPointerType

  subroutine getScalar(this, value)
     class(TYPE),intent(in) :: this
     class(*),intent(inout) :: value
     call stop_model('value should not be real scalar',255)
  end subroutine getScalar

  subroutine get1DValue(this, value)
     class(TYPE),intent(in) :: this
     class(*),intent(inout) :: value(:)

     select type (value)
     type is (real(KIND=DP))
        value=this%value
     class default
        call stop_model('should be 1D real DP type of value.',255)
     end select
  end subroutine get1DValue

  subroutine setScalar(this, value)
     class(TYPE),intent(inout) :: this
     class(*),intent(in) :: value
     call stop_model('value should not be real scalar',255)
  end subroutine setScalar

  subroutine set1DValue(this, value)
     class(TYPE),intent(inout) :: this
     class(*),intent(in) :: value(:)

     if(this%dims(1) /= size(value,1)) then
        call stop_model('should not change dimension through setValue call')
     endif
     select type (value)
     type is (real(KIND=DP))
        this%value=value
     class default
        call stop_model('should be 1D real DP type of value.',255)
     end select
  end subroutine set1DValue


  logical function equals(this, b)
    class (RealDP1dValue), intent(in) :: this
    class (AbstractValue), intent(in) :: b

    select type (p => b)
    class is (RealDP1dValue)
      if (all(this%value == p%value)) then
        equals = .true.
      else
        equals = .false.
      end if
    class default
      equals = .false.
    end select

  end function equals

  subroutine printIt(this)
    class (RealDP1dValue), intent(in) :: this
    print*,'  Type:  ', 'RealDP1dValue'
    print*,'  Value: <', this%value,'>'
    print*,'--------------'
  end subroutine printIt

  function toString(this) result(string)
    use StringUtilities_mod, only: toStringElemental => toString
    class (RealDP1dValue), intent(in) :: this
    character(len=MAX_LEN_LINE) :: string

    string = join(reshape(toStringElemental(this%value),(/size(this%value)/)),', ')

  contains

    function join(strArray, separator) result(string)
      character(len=*), intent(in) :: strArray(:)
      character(len=*), intent(in) :: separator
      character(len=MAX_LEN_LINE) :: string

      integer :: i
      string = trim(strArray(1))
      do i = 2, size(strArray)
        string = trim(string) // trim(separator) // trim(strArray(i))
      end do
    end function join

  end function toString

  subroutine writeUnformatted(this, unit)
    class (RealDP1dValue), intent(in) :: this
    integer, intent(in) :: unit

    write(unit) shape(this%value)
    
    write(unit) this%value

  end subroutine writeUnformatted

  function readUnformatted(this, unit) result(new)
    
    class (RealDP1dValue), intent(in) :: this
    integer, intent(in) :: unit
    class (AbstractValue), pointer :: new

    integer :: rank
    real(kind=DP), pointer :: value (:)
    

    integer :: attributeShape(1)

    read(unit) attributeShape
    allocate(value(attributeShape(1)))
    
    

    read(unit) value

    

    allocate(new, source=newValue(value))
    deallocate(value)
    

  end function readUnformatted

  subroutine clear(this)
    class (RealDP1dValue), intent(inout) :: this
    deallocate(this%value)
  end subroutine clear

end module RealDP1dValue_mod
#undef TYPE


module StringValue_mod
  use AbstractValue_mod
  
  implicit none
  private

#define TYPE StringValue

  public :: TYPE
  public :: newValue
  public :: toType
  public :: toPointer
  public :: assignment(=)

  type, extends(AbstractValue) :: TYPE
    character(len=MAX_LEN_ATTRIBUTE_STRING) :: value
  contains   
    procedure :: equals
    procedure :: clear
    procedure :: print => printIt ! gfortran workaround
    procedure :: writeUnformatted
    procedure :: readUnformatted
    procedure :: toString
    procedure :: getScalar
    procedure :: get1DValue
    procedure :: setScalar
    procedure :: set1DValue
  end type TYPE

  interface newValue
    module procedure constructor
  end interface

  interface assignment(=)
     module procedure toType_
  end interface

  interface toType
     module procedure toType_
  end interface

  interface toPointer
     module procedure toPointerType
  end interface toPointer

contains

  function constructor(value) result(entry)
    type (TYPE) :: entry
    character(len=*), intent(in) :: value 
    

    entry%value = value
    entry%rank = 0
    entry%name = "string"
    allocate(entry%dims(0))
    

  end function constructor

  subroutine toType_(value, entry)
     character(len=MAX_LEN_ATTRIBUTE_STRING) , intent(inout) :: value 
    class (AbstractValue), intent(in) :: entry

    integer :: i

    select type (entry)
    type is (TYPE)
        value = entry%value



    class default
      call stop_model('Illegal conversion of StringValue.',255)
    end select
  end subroutine toType_

  function toPointerType(entry, cast) result(ptr)
    character(len=MAX_LEN_ATTRIBUTE_STRING), pointer :: ptr 
    class (AbstractValue), target, intent(in) :: entry
    character(len=MAX_LEN_ATTRIBUTE_STRING) :: cast 

    select type (q => entry)
    type is (StringValue)
       ptr => q%value
    class default
      call stop_model('Illegal association of StringValue.',255)
       print*,'Illegal association of StringValue.'
    end select
  end function toPointerType

  subroutine getScalar(this, value)
     class(TYPE),intent(in) :: this
     class(*),intent(inout) :: value
     select type (value)
     type is (character(len=*))
        value=this%value
     class default
        call stop_model('should be type character(*)',255)
     end select
  end subroutine getScalar

  subroutine get1DValue(this, value)
     class(TYPE),intent(in) :: this
     class(*),intent(inout) :: value(:)
     call stop_model('value should not be 1D chars',255)
  end subroutine get1DValue

  subroutine setScalar(this, value)
     class(TYPE),intent(inout) :: this
     class(*),intent(in) :: value
     select type (value)
     type is (character(len=*))
        this%value=value
     class default
        call stop_model('should be type character(*)',255)
     end select
  end subroutine setScalar

  subroutine set1DValue(this, value)
     class(TYPE),intent(inout) :: this
     class(*),intent(in) :: value(:)
     call stop_model('value should not be 1D chars',255)
  end subroutine set1DValue

  logical function equals(this, b)
    class (StringValue), intent(in) :: this
    class (AbstractValue), intent(in) :: b

    select type (p => b)
    class is (StringValue)
      if ((this%value == p%value)) then
        equals = .true.
      else
        equals = .false.
      end if
    class default
      equals = .false.
    end select

  end function equals

  subroutine printIt(this)
    class (StringValue), intent(in) :: this
    print*,'  Type:  ', 'StringValue'
    print*,'  Value: <', this%value,'>'
    print*,'--------------'
  end subroutine printIt

  function toString(this) result(string)
    use StringUtilities_mod, only: toStringElemental => toString
    class (StringValue), intent(in) :: this
    character(len=MAX_LEN_LINE) :: string

    string = toStringElemental(this%value)

  contains

    function join(strArray, separator) result(string)
      character(len=*), intent(in) :: strArray(:)
      character(len=*), intent(in) :: separator
      character(len=MAX_LEN_LINE) :: string

      integer :: i
      string = trim(strArray(1))
      do i = 2, size(strArray)
        string = trim(string) // trim(separator) // trim(strArray(i))
      end do
    end function join

  end function toString

  subroutine writeUnformatted(this, unit)
    class (StringValue), intent(in) :: this
    integer, intent(in) :: unit

    
    write(unit) len_trim(this%value)
    write(unit) this%value

  end subroutine writeUnformatted

  function readUnformatted(this, unit) result(new)
    use StringUtilities_mod, only: forceTrim
    class (StringValue), intent(in) :: this
    integer, intent(in) :: unit
    class (AbstractValue), pointer :: new

    integer :: rank
    character(len=MAX_LEN_ATTRIBUTE_STRING), pointer :: value 
    integer, pointer :: lengths 

    allocate(value , lengths)
    read(unit) lengths

    read(unit) value

    call forceTrim(value,lengths)

    allocate(new, source=newValue(value))
    deallocate(value)
    deallocate(lengths)

  end function readUnformatted

  subroutine clear(this)
    class (StringValue), intent(inout) :: this
    
  end subroutine clear

end module StringValue_mod
#undef TYPE


module String1dValue_mod
  use AbstractValue_mod
  use StringValue_mod
  implicit none
  private

#define TYPE String1dValue

  public :: TYPE
  public :: newValue
  public :: toType
  public :: toPointer
  public :: assignment(=)

  type, extends(AbstractValue) :: TYPE
    character(len=MAX_LEN_ATTRIBUTE_STRING), allocatable :: value (:)

  contains   
    procedure :: equals
    procedure :: clear
    procedure :: print => printIt ! gfortran workaround
    procedure :: writeUnformatted
    procedure :: readUnformatted
    procedure :: toString
    procedure :: getScalar
    procedure :: get1DValue
    procedure :: setScalar
    procedure :: set1DValue
  end type TYPE

  interface newValue
    module procedure constructor
  end interface

  interface assignment(=)
     module procedure toType_
  end interface

  interface toType
     module procedure toType_
  end interface

  interface toPointer
     module procedure toPointerType
  end interface toPointer

contains

  function constructor(value) result(entry)
    type (TYPE) :: entry
    character(len=*), intent(in) :: value (:)
    allocate(entry%value (size(value,1)))

    entry%value = value
    entry%rank = 1
    entry%name = "string1D"
    allocate(entry%dims(1))
    entry%dims(1)= size(value,1)

  end function constructor

  subroutine toType_(value, entry)
    character(len=MAX_LEN_ATTRIBUTE_STRING) , allocatable, intent(inout) :: value (:)
    class (AbstractValue), intent(in) :: entry

    integer :: i

    select type (entry)
    type is (TYPE)
#ifdef __GFORTRAN__
        value = entry%value
#else
        allocate(value, source=entry%value)
#endif

    class default
      call stop_model('Illegal conversion of String1dValue.',255)
    end select
  end subroutine toType_

  function toPointerType(entry, cast) result(ptr)
    character(len=MAX_LEN_ATTRIBUTE_STRING), pointer :: ptr (:)
    class (AbstractValue), target, intent(in) :: entry
    character(len=MAX_LEN_ATTRIBUTE_STRING) :: cast (:)

    select type (q => entry)
    type is (String1dValue)
       ptr => q%value
    class default
       call stop_model('Illegal association of String1dValue.',255)
    end select
  end function toPointerType

  subroutine getScalar(this, value)
     class(TYPE),intent(in) :: this
     class(*),intent(inout) :: value
     call stop_model('value should not be character(*)',255)
  end subroutine getScalar

  subroutine get1DValue(this, value)
     class(TYPE),intent(in) :: this
     class(*),intent(inout) :: value(:)

     select type (value)
     type is (character(len=*))
        value=this%value
     class default
        call stop_model('should be 1D character(*)',255)
     end select
  end subroutine get1DValue

  subroutine setScalar(this, value)
     class(TYPE),intent(inout) :: this
     class(*),intent(in) :: value
     call stop_model('value should not be character(*)',255)
  end subroutine setScalar

  subroutine set1DValue(this, value)
     class(TYPE),intent(inout) :: this
     class(*),intent(in) :: value(:)

     if(this%dims(1) /= size(value,1)) then
        call stop_model('should not change dimension through setValue call')
     endif
     select type (value)
     type is (character(len=*))
        this%value=value
     class default
        call stop_model('should be 1D character(*)',255)
     end select
  end subroutine set1DValue

  logical function equals(this, b)
    class (String1dValue), intent(in) :: this
    class (AbstractValue), intent(in) :: b

    select type (p => b)
    class is (String1dValue)
      if (all(this%value == p%value)) then
        equals = .true.
      else
        equals = .false.
      end if
    class default
      equals = .false.
    end select

  end function equals

  subroutine printIt(this)
    class (String1dValue), intent(in) :: this
    print*,'  Type:  ', 'String1dValue'
    print*,'  Value: <', this%value,'>'
    print*,'--------------'
  end subroutine printIt

  function toString(this) result(string)
    use StringUtilities_mod, only: toStringElemental => toString
    class (String1dValue), intent(in) :: this
    character(len=MAX_LEN_LINE) :: string

    string = join(reshape(toStringElemental(this%value),(/size(this%value)/)),', ')

  contains

    function join(strArray, separator) result(string)
      character(len=*), intent(in) :: strArray(:)
      character(len=*), intent(in) :: separator
      character(len=MAX_LEN_LINE) :: string

      integer :: i
      string = trim(strArray(1))
      do i = 2, size(strArray)
        string = trim(string) // trim(separator) // trim(strArray(i))
      end do
    end function join

  end function toString

  subroutine writeUnformatted(this, unit)
    class (String1dValue), intent(in) :: this
    integer, intent(in) :: unit

    write(unit) shape(this%value)
    write(unit) len_trim(this%value)
    write(unit) this%value

  end subroutine writeUnformatted

  function readUnformatted(this, unit) result(new)
    use StringUtilities_mod, only: forceTrim
    class (String1dValue), intent(in) :: this
    integer, intent(in) :: unit
    class (AbstractValue), pointer :: new

    integer :: rank
    character(len=MAX_LEN_ATTRIBUTE_STRING), pointer :: value (:)
    integer, pointer :: lengths (:)

    integer :: attributeShape(1)

    read(unit) attributeShape
    allocate(value(attributeShape(1)))
    allocate(lengths(attributeShape(1)))
    
    read(unit) lengths

    read(unit) value

    call forceTrim(value,lengths)

    allocate(new, source=newValue(value))
    deallocate(value)
    deallocate(lengths)

  end function readUnformatted

  subroutine clear(this)
    class (String1dValue), intent(inout) :: this
    deallocate(this%value)
  end subroutine clear

end module String1dValue_mod
#undef TYPE

module  FTLValueTypeMap_mod
   use AbstractValue_mod
#define _key_string_deferred
#define _key_equal_defined
#define _Key_less_than_defined

#define _value class(AbstractValue)
#define _value_allocatable
#define _alt
#include "templates/map.inc"
end module FTLValueTypeMap_mod

module ValueTypeMap_mod
   use AbstractValue_mod
   use integerValue_mod
   use integer1dValue_mod
   use logicalValue_mod
   use logical1dValue_mod
   use RealDPValue_mod
   use RealDP1dValue_mod
   use StringValue_mod
   use String1dValue_mod
   use FTLValueTypeMap_mod
   implicit none

   type,extends(Map) :: ValueTypeMap
   contains
     procedure ::  initValueTypeMap
     generic :: init=>initValueTypeMap

     procedure :: copyVmap
     generic   :: assignment(=) => copyVmap
   end type

contains

   subroutine initValueTypeMap(this)
     class(ValueTypeMap),intent(inout) :: this 
     call this%insert('integer',newValue(0))
     call this%insert('integer1D',newValue([1,2]))
     call this%insert('logical',newValue(.true.))
     call this%insert('logical1D',newValue([.false.,.false.]))
     call this%insert('realDP',newValue(0.5d0))
     call this%insert('realDP1D',newValue([0.6d0,0.7d0]))
     call this%insert('string',newValue('a'))
     call this%insert('string1D',newValue(['c','d']))
   end subroutine
  
   subroutine copyVmap(to,from)
     class(ValueTypeMap),intent(inout) :: to 
     class(ValueTypeMap),intent(in) :: from
     call to%deepCopy(from)
   end subroutine

end module ValueTypeMap_mod

module Values_mod
  use AbstractValue_mod
  use integerValue_mod
  use integer1dValue_mod
  use logicalValue_mod
  use logical1dValue_mod
  use RealDPValue_mod
  use RealDP1dValue_mod
  use StringValue_mod
  use String1dValue_mod
  use ValueTypeMap_mod
  implicit none
end module Values_mod  

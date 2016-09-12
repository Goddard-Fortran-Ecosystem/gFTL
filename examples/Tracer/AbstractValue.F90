module AbstractValue_mod
  implicit none
  private

  public :: AbstractValue
  public :: DP
  public :: SP
  public :: MAX_LEN_KEY
  public :: MAX_LEN_LINE
  public :: MAX_LEN_ATTRIBUTE_STRING 
  
  integer, parameter :: DP = selected_real_kind(14)
  integer, parameter :: SP = selected_real_kind(6)
  integer, parameter :: MAX_LEN_KEY = 32
  integer, parameter :: MAX_LEN_ATTRIBUTE_STRING = 80
  integer, parameter :: MAX_LEN_LINE = 1000

  type, abstract :: AbstractValue
     integer :: rank
     integer,allocatable :: dims(:)
     character(len=MAX_LEN_KEY) :: name
  contains
    procedure(equals), deferred :: equals
    procedure(print), deferred :: print
    procedure(toString), deferred :: toString
    procedure(writeUnformatted), deferred :: writeUnformatted
    procedure(readUnformatted), deferred :: readUnformatted
    procedure(clear), deferred :: clear

    procedure(get0d),deferred :: getScalar
    procedure(get1d),deferred :: get1DValue
    generic :: getValue=> getScalar, get1DValue

    procedure(set0d),deferred :: setScalar
    procedure(set1d),deferred :: set1DValue
    generic :: setValue=> setScalar, set1DValue

  end type AbstractValue


  abstract interface

    subroutine get0d(this,value)
       import AbstractValue
       class(AbstractValue), intent(in) :: this
       class(*),intent(inout) :: value
    end subroutine get0d

    subroutine get1d(this,value)
       import AbstractValue
       class(AbstractValue), intent(in) :: this
       class(*),intent(inout) :: value(:)
    end subroutine get1d

    subroutine set0d(this,value)
       import AbstractValue
       class(AbstractValue), intent(inout) :: this
       class(*),intent(in) :: value
    end subroutine set0d

    subroutine set1d(this,value)
       import AbstractValue
       class(AbstractValue), intent(inout) :: this
       class(*),intent(in) :: value(:)
    end subroutine set1d

    logical function equals(this, b)
      import AbstractValue
      class (AbstractValue), intent(in) :: this
      class (AbstractValue), intent(in) :: b
    end function equals

    function toString(this) result(string)
      import AbstractValue, MAX_LEN_LINE
      class (AbstractValue), intent(in) :: this
      character(len=MAX_LEN_LINE) :: string
    end function toString

    subroutine print(this)
      import AbstractValue
      class (AbstractValue), intent(in) :: this
    end subroutine print

    subroutine writeUnformatted(this, unit)
      import AbstractValue
      class (AbstractValue), intent(in) :: this
      integer, intent(in) :: unit
    end subroutine writeUnformatted

    function readUnformatted(this, unit) result(new)
      import AbstractValue
      class (AbstractValue), intent(in) :: this
      integer, intent(in) :: unit
      class (AbstractValue), pointer :: new
    end function readUnformatted

    subroutine clear(this)
      import AbstractValue
      class (AbstractValue), intent(inout) :: this
    end subroutine clear

  end interface

end module AbstractValue_mod

module Tracer_mod
  use Values_mod
  use AttributeMap_mod
  use TracerSurfaceSource_mod, only: TracerSurfaceSource
  use TracerSource_mod, only: TracerSource3D
  implicit none
  private

  public :: Tracer       ! derived type
  public :: clear

  public :: writeUnformatted
  public :: readUnformattedTracer

  public :: findSurfaceSources
  public :: addSurfaceSource
  public :: readSurfaceSources

  public :: NTSURFSRCMAX

!@var ntsurfsrcmax maximum number of surface 2D sources/sinks
  integer, parameter :: NTSURFSRCMAX=16
!@var nt3Dsrcmax maximum number of 3D tracer sources/sinks
  integer, parameter :: NT3DSRCMAX=7

  type :: Tracer
!!$    private
    character(len=MAX_LEN_KEY) :: name
    integer :: ntSurfSrc = 0
    type (TracerSurfaceSource), allocatable, dimension(:) :: surfaceSources
    type (TracerSource3D), allocatable, dimension(:)  :: sources3D
    type (AttributeMap) :: attributes
  contains
    procedure :: initTracer
    generic :: init=>initTracer

    procedure :: addAttribute0d
    procedure :: addAttribute1d
    generic   :: addAttribute=>addAttribute0d,addAttribute1d

    procedure :: getAttribute0d
    procedure :: getAttribute1d
    generic   :: getAttribute=>getAttribute0d,getAttribute1d

    procedure :: setAttribute0d
    procedure :: setAttribute1d
    generic   :: setAttribute=>setAttribute0d,setAttribute1d
    
    procedure :: hasAttribute
    procedure :: getName

    procedure :: copyTracer
    generic :: assignment(=) =>copyTracer

    procedure :: clearTracer
    generic :: clear=>clearTracer

    procedure :: printIt
    generic :: print => printIt

    procedure :: equals
    generic :: operator(==) => equals

  end type Tracer

  interface writeUnformatted
    module procedure writeUnformatted_tracer
  end interface

  interface clear
    module procedure clearTracer
  end interface

contains

  subroutine initTracer(this,name)
    class (Tracer),intent(inout) :: this
    character(len=*) :: name
    this%name =  trim(name)
    call this%attributes%init()
    allocate(this%surfaceSources(NTSURFSRCMAX))
    allocate(this%sources3D(NT3DSRCMAX))
  end subroutine initTracer

  subroutine addAttribute0d(this,name,value)
     use AbstractValue_mod
     class (Tracer),intent(inout) :: this
     character(len=*),intent(in) :: name
     class(*),intent(in) :: value
     call this%attributes%insertValue(name,value)
  end subroutine addAttribute0d

  subroutine addAttribute1d(this,name,value)
     class (Tracer),intent(inout) :: this
     character(len=*),intent(in) :: name
     class(*),intent(in) :: value(:)
     character(len=MAX_LEN_ATTRIBUTE_STRING),allocatable :: s1(:)
     integer :: n

     ! work around for gfortran
     select type (value)
     type is (character(len=*))
        n = size(value)
        allocate(s1(n))
        s1=value
        call this%attributes%insertValue(trim(name),s1)
        deallocate(s1)
        return
     class default
     end select

     call this%attributes%insertValue(trim(name),value)

  end subroutine addAttribute1d

  function getAttribute0d(this,name,value) result (res)
     use AbstractValue_mod
     class (Tracer),intent(inout) :: this
     character(len=*),intent(in) :: name
     class(*),intent(inout) :: value
     logical :: res
     res = this%attributes%getValue(trim(name),value)
  end function getAttribute0d

  function getAttribute1d(this,name,value) result(res)
     class (Tracer),intent(inout) :: this
     character(len=*),intent(in) :: name
     class(*),intent(inout) :: value(:)
     logical :: res
     character(len=MAX_LEN_ATTRIBUTE_STRING),allocatable :: s1(:)
     integer :: n

     ! work around for gfortran
     select type (value)
     type is (character(len=*))
        n = size(value)
        allocate(s1(n))
        res = this%attributes%getValue(trim(name),s1)
        value=s1
        deallocate(s1)
        return
     class default
     end select

     res= this%attributes%getValue(trim(name),value)

  end function getAttribute1d

  subroutine setAttribute0d(this,name,value)
     use AbstractValue_mod
     class (Tracer),intent(inout) :: this
     character(len=*),intent(in) :: name
     class(*),intent(in) :: value
     call this%attributes%setValue(trim(name),value)
  end subroutine setAttribute0d

  subroutine setAttribute1d(this,name,value)
     class (Tracer),intent(inout) :: this
     character(len=*),intent(in) :: name
     class(*),intent(in) :: value(:)
     character(len=MAX_LEN_ATTRIBUTE_STRING),allocatable :: s1(:)
     integer :: n

     ! work around for gfortran
     select type (value)
     type is (character(len=*))
        n = size(value)
        allocate(s1(n))
        s1=value
        call this%attributes%setValue(trim(name),s1)
        deallocate(s1)
        return
     class default
     end select

     call this%attributes%setValue(trim(name),value)

  end subroutine setAttribute1d

  function hasAttribute(this,attrName) result(l)

     class(Tracer),intent(in):: this
     character(len=*),intent(in) :: attrName
     logical :: l
     l=.true.
     if( this%attributes%find(trim(attrName)) == this%attributes%end()) l=.false.

  end function hasAttribute

  subroutine copyTracer(copy,original)

    class(Tracer),intent(inout) :: copy
    class(Tracer), intent(in) :: original
    integer :: n

    copy%attributes = original%attributes
    copy%ntSurfSrc = original%ntSurfSrc
    copy%name = original%name
    n = size(original%surfaceSources,1)
    allocate(copy%surfaceSources(1:n), source=original%surfaceSources)
    n = size(original%sources3D,1)
    allocate(copy%sources3D(1:n),source = original%sources3D)

  end subroutine copyTracer

  function getName(this) result (name)
    class (Tracer), target, intent(in) :: this
    character(len=MAX_LEN_ATTRIBUTE_STRING) :: name
    name=this%name
  end function getName

  subroutine writeUnformatted_tracer(this, unit)
!@sum Write a tracer to a unit attached to an unformatted sequential file.
    type (Tracer), intent(in) :: this
    integer, intent(in) :: unit

    call this%attributes%writeUnformatted(unit)
    
  end subroutine writeUnformatted_tracer

  subroutine readUnformattedTracer(this, unit)
!@sum Read a bundle to a unit attached to an unformatted sequential file.
!!$    use Dictionary_mod, only: readUnformatted
    type (Tracer), intent(inout) :: this
    integer, intent(in) :: unit
    call this%attributes%readUnformatted(unit)
  end subroutine readUnformattedTracer

  subroutine clearTracer(this)
    class(Tracer), intent(inout) :: this
    call this%attributes%clear()
    !deallocate(this%surfaceSources)
    !deallocate(this%sources3D)
  end subroutine clearTracer

  subroutine findSurfaceSources(trcer, checkname, sect_name) 
!@sum reads headers from emission files to return
!@+ source names and determine the number of sources
!@+ from the number of files in the rundeck of the form:
!@+ trname_##. Then assigns each source to sector(s),
!@+ based on definitions in the rundeck.
!@auth Greg Faluvegi

    use GenericType_mod

    USE SpecialIO_mod, only: write_parallel
    use MpiSupport_mod, only: am_i_root

    implicit none

!@var nsrc number of source to define ntsurfsrc(n)
    type (Tracer), intent(inout) :: trcer
    logical, intent(in) :: checkName
    character*10, intent(in):: sect_name(:)

    integer :: n
    character*80 :: fname
    character(len=300) :: out_line
    logical :: fileExists
    integer :: nsrc

    ! loop through potential number of surface sources, checking if
    ! those files exist. If they do, obtain the source name by reading
    ! the header. If not, the number of sources for this tracer has 
    ! been reached.

    nsrc=0

    loop_n: do n = 1, ntsurfsrcmax

      fname = addIntegerSuffix(getName(trcer), n)
      inquire(file=trim(fname), exist=fileExists)
      if (am_i_root()) print*,'name: ', trim(fname), fileExists

      if (fileExists) then
        nsrc=nsrc+1
        call addSourceFromFile(trcer, fname)
      else
        exit loop_n
      endif
    enddo loop_n

    ! and make sure there isn't a skip:

    n=n+1
    fname = addIntegerSuffix(getName(trcer), n)
    inquire(file=fname,exist=fileExists)

    if (fileExists) then
      write(out_line,*)'problem in findSurfaceSources.', &
           &        ' Possibly missing source? n=',n-1
      call write_parallel(trim(out_line))
      call stop_model(trim(out_line),255)
    endif

  contains

    subroutine addSourceFromFile(trcer, fileName)
      use TracerSurfaceSource_mod, only: initSurfaceSource
      type (Tracer), intent(inout) :: trcer
      character(len=*), intent(in) :: fileName

      trcer%ntSurfSrc = trcer%ntSurfSrc + 1
      call initSurfaceSource(trcer%surfaceSources(trcer%ntSurfSrc),  &
           &     getName(trcer), fileName, sect_name, checkname)
    end subroutine addSourceFromFile

  end subroutine findSurfaceSources

  ! Use this routine to add a new surface source that
  ! is manipulated by custom logic elsewhere.
  ! Optional sourcename is only used by diagnostics
  subroutine addSurfaceSource(this, sourceName)
    type (Tracer), intent(inout) :: this
    character(len=*), intent(in) :: sourceName

    this%ntSurfSrc = this%ntSurfSrc + 1
    this%surfaceSources(this%ntSurfSrc)%sourceName = sourceName

  end subroutine addSurfaceSource

!TODO - move to string utilities
  function addIntegerSuffix(tracerName, n) result(fullName)
    character(len=*), intent(in) :: tracerName
    character(len=len_trim(tracerName)+3) :: fullName
    integer, intent(in) :: n
    
    character(len=2) :: suffix
      
    write(suffix,'(I2.2)') n
    fullName = trim(tracerName) // '_' // suffix
  end function addIntegerSuffix

  subroutine readSurfaceSources(trcer, n,nsrc,xyear,xday,checkname,itime,itime_tr0,sfc_src)
!@sum reads surface (2D generally non-interactive) sources
!@auth Jean Lerner/Greg Faluvegi
    USE DOMAIN_DECOMP_ATM, only: GRID
    use TracerSurfaceSource_mod, only: readSurfaceSource
    type (Tracer), target, intent(inout) :: trcer
    integer, intent(in) :: nsrc,n
    integer, intent(in) :: xyear, xday
    logical, intent(in) :: checkname
    integer, intent(in) :: itime
    integer, intent(in) :: itime_tr0
    real*8, intent(inout) :: sfc_src(grid%i_strt_halo:,grid%j_strt_halo:,:,:)

    integer :: ns

    if (itime < itime_tr0) return
    if (nsrc <= 0) return

    do ns=1,nsrc
      call readSurfaceSource(trcer%surfaceSources(ns), addIntegerSuffix(getName(trcer), ns), checkname, sfc_src(:,:,n,ns), &
           & xyear, xday)
    enddo

    return

  end subroutine readSurfaceSources

  subroutine toTracer(pType, pClass)
    type (Tracer), pointer, intent(out) :: pType
    class (Tracer), target, intent(in) :: pClass

    select type (p => pClass)
    type is (Tracer)
      pType => p
    class default
      call stop_model('Illegal conversion in Tracer_mod.',255)
    end select
    
  end subroutine toTracer

  subroutine printIt(this)
    class(Tracer),intent(inout) :: this
    print*,"Tracer name : ",this%name
    call this%attributes%print() 
  end subroutine printIt

  logical function equals( this, b)
     class(Tracer),intent(in) :: this
     type(Tracer),intent(in) :: b
     equals = (this%attributes == b%attributes)
  end function equals

end module Tracer_mod

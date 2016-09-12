#include "templates/all_template_macros.inc"


! This constructor is needed in situations where an empty dictionary needs to be
! passed to a procedure.  Prevents the need of declaring a local variable.
      function __PROC(new_map_empty)() result(m)
         type (__map) :: m

         if (.false.) print*,shape(m) ! avoid compiler warnings about unused
         
      end function __PROC(new_map_empty)

#ifdef _alt
      function __PROC(new_map_from_pair_array)(pairs) result(m)
         type (__map) :: m
         type (__pair), intent(in) :: pairs(:)
         
         integer :: i

         do i = 1, size(pairs)
            call m%insert(pairs(i))
         end do
         
      end function __PROC(new_map_from_pair_array)
#endif

! =======================
!  empty
! =======================
      logical function __PROC(empty)(this) result(isEmpty)
         class (__map), intent(in) :: this

         isEmpty = this%tree%empty()

      end function __PROC(empty)


! =======================
!  size
! =======================
      function __PROC(size)(this) result(size)
         integer(kind=SIZE_kind) :: size
         class (__map), intent(in) :: this

         size = this%tree%size()

      end function __PROC(size)


! =======================
!  max_size
! =======================
!  limited by 32 bit integer in terms of result
      function __PROC(max_size)() result(max_size)
         integer(kind=SIZE_KIND) :: max_size

         max_size = huge(1_SIZE_KIND)

      end function __PROC(max_size)


! =======================
!  insert
! =======================
      subroutine __PROC(insert_key_value)(this, key, value)
         class (__map), intent(inout) :: this
         __key_declare_dummy, intent(in) :: key
         __value_declare_dummy, intent(in) :: value

         type (__pair) :: p

         __KEY_ASSIGN(p%key, key)
         __VALUE_ASSIGN(p%value, value)

         call this%tree%insert(p)

      end subroutine __PROC(insert_key_value)


      subroutine __PROC(insert_pair)(this, p)
         class (__map), intent(inout) :: this
         type (__pair), intent(in) :: p

         call this%tree%insert(p)

      end subroutine __PROC(insert_pair)

! =======================
!  get
! =======================
      function __PROC(get)(this, key, value) result(res)
      class(__map), target, intent(in) :: this
      __key_declare_dummy :: key
      __value_declare_result, pointer, intent(out) :: value
      logical :: res
      type(__pair) :: p
      type(__siterator) :: it
#ifdef _alt
      type(__pair), pointer :: q
#endif

      __KEY_ASSIGN(p%key, key)
      it=this%tree%find(p)
      res= (it/=this%tree%end())
#ifdef _alt
      if (res) then
         q => it%value()
         value => q%value
      end if
#else
      if (res) value=>it%node%value%value
#endif
      return
      end function __PROC(get)

! =======================
!  set
! =======================
      subroutine __PROC(set)(this, key, value)
      class(__map), intent(inout) :: this
      __key_declare_dummy, intent(in) :: key
      __value_declare_dummy, intent(in) :: value
      type(__pair) :: p

      __KEY_ASSIGN(p%key, key)
      __VALUE_ASSIGN(p%value, value)

      call this%tree%insert(p)
      return

      end subroutine __PROC(set)

! =======================
!  of - grows map if key does not exist
!  Analog of C++ [] operator.
! =======================
      function __PROC(of)(this, key) result(res)
      class(__map), target, intent(inout) :: this
      __key_declare_dummy, intent(in) :: key
#ifdef _value_deferred
      character(len=:), allocatable :: res         ! bug in gfortran 4.9.1
#else
      __value_declare_result, pointer :: res
#endif
      type(__pair) :: p
#ifdef _alt
      integer(kind=SIZE_KIND) :: ref
#else
      type(node), pointer :: ref
#endif
      logical :: isNew
#ifdef _alt
      type(__pair), pointer :: q
#endif

      __KEY_ASSIGN(p%key, key)
      call this%tree%insert(p, ref=ref, isNew=isNew)
      if (.not. isNew) then
#ifdef _alt
         q => this%tree%items%at(ref) 
#  ifdef _value_string
         res= q%value
#  else
         res=>q%value
#  endif
#else
#  ifdef _value_string
         res=ref%value%value
#  else
         res=>ref%value%value
#  endif
#endif
      else
         res => null()
      end if
      return
      end function __PROC(of)

! =======================
!  at
! =======================
      function __PROC(at)(this, key) result(res)
      class(__map), target, intent(in) :: this
      __key_declare_dummy, intent(in) :: key
#ifdef _value_deferred
      character(len=:), allocatable :: res         ! bug in gfortran 4.9.1
#else
      __value_declare_result, pointer :: res
#endif

      type (__iterator) :: iter

      iter = this%find(key)
#ifdef _alt
      if (iter%setIter%current == UNINITIALIZED) then
         ! throw exception
         res => null()
         return
      end if
#else
      if (.not. associated(iter%setIter%node)) then
         ! throw exception
         res => null()
         return
      end if
#endif

#  ifdef _value_string
      res= iter%value()
#  else
      res=> iter%value()
#  endif
      return
      end function __PROC(at)



! =======================
!  erase
! =======================
      subroutine __PROC(erase_one)(this, iter)
      class(__map), intent(inout) :: this
      type(__iterator), intent(inout) :: iter

      call this%tree%erase(iter%setIter)

      end subroutine __PROC(erase_one)


! =======================
!  clear
! =======================
      subroutine __PROC(clear)(this)
      class(__map), intent(inout) :: this

      call this%tree%clear()

      end subroutine __PROC(clear)



! =======================
!  begin
! =======================
      function __PROC(begin)(this) result(iter)
         class(__map), target, intent(in) :: this
         type (__iterator) :: iter

         iter%reference => this
         iter%setIter = this%tree%begin()

      end function __PROC(begin)


! =======================
!  end
! =======================
      function __PROC(end)(this) result(iter)
         class(__map), target, intent(in) :: this
         type (__iterator) :: iter

         iter%reference => this
         iter%setIter = this%tree%end()

      end function __PROC(end)


! =======================
!  find
! =======================
      function __PROC(find)(this, key) result(iter)
         type (__iterator) :: iter
         class(__map), target, intent(in) :: this
         __key_declare_dummy, intent(in) :: key

         type (__pair) :: p

         __KEY_ASSIGN(p%key, key)

         iter%reference => this
         iter%setIter = this%tree%find(p)

      end function __PROC(find)


! =======================
!  count
! =======================

      function __PROC(count)(this, key) result(count)
         integer(kind=SIZE_KIND) :: count
         class(__map), intent(in) :: this
         __key_declare_dummy, intent(in) :: key

         type (__pair) :: p

         __KEY_ASSIGN(p%key, key)

         count = this%tree%count(p)

      end function __PROC(count)


! =======================
!  copyFrom
! =======================

      subroutine __PROC(deepCopy)(this, original)
         class(__map), intent(out) :: this
         class(__map), intent(in) :: original

         call this%tree%deepCopy(original%tree)

      end subroutine __PROC(deepCopy)
      
! =======================
!  value
! =======================
      function __PROC(value)(this) result(res)
         class(__iterator), target, intent(in) :: this
         __value_declare_result, pointer :: res

         type(__pair), pointer :: p

         p => this%setIter%value()
         res => p%value

      end function __PROC(value)

! =======================
!  key
! =======================
      function __PROC(key)(this) result(res)
         class(__iterator), target, intent(in) :: this
         __key_declare_result, pointer :: res

         type(__pair), pointer :: p

         p => this%setIter%value()
         res => p%key

      end function __PROC(key)


! =======================
!  operator(==)
! =======================
      logical function __PROC(iter_equal)(this, other) result(equal)
         class(__iterator), intent(in) :: this
         type(__iterator), intent(in) :: other

         equal = (this%setIter == other%setIter)

      end function __PROC(iter_equal)


! =======================
!  operator(/=)
! =======================
      logical function __PROC(iter_not_equal)(this, other)                          &
     &   result(not_equal)
         class(__iterator), intent(in) :: this
         type(__iterator), intent(in) :: other

         not_equal = .not. (this == other)
      end function __PROC(iter_not_equal)


! =======================
!  next
! =======================
      subroutine __PROC(next)(this)
         class(__iterator), intent(inout) :: this

         call this%setIter%next()
      end subroutine __PROC(next)


! =======================
!  previous
! =======================
      subroutine __PROC(previous)(this)
         class(__iterator), intent(inout) :: this

         call this%setIter%prev()
      end subroutine __PROC(previous)



#ifdef _DUMP_MAP
! =======================
!  mapdump
! =======================
      subroutine mapdump(this)
      class(__map), intent(inout) :: this
      type(__siterator) :: it
      type(__pair), pointer :: p

      it=this%tree%begin()
      do while (it%good())
        p=>it%value()
        write(*, *)p%key, '=>',p%value,'+'
        call it%next
      end do
      return
      end subroutine mapdump
#endif

#include "templates/all_template_macros_undefs.inc"

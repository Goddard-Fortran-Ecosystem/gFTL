      function __PROC(iter_get)(this) result (item)
         class (__iterator), intent(in) :: this

         __type_declare_result, pointer :: item
         item => __GET(this%elements(this%currentIndex))

      end function __PROC(iter_get)


      subroutine __PROC(iter_next)(this)
         class (__iterator), intent(inout) :: this
         this%currentIndex = this%currentIndex + 1
      end subroutine __PROC(iter_next)


      subroutine __PROC(iter_previous)(this)
         class (__iterator), intent(inout) :: this
         this%currentIndex = this%currentIndex - 1
      end subroutine __PROC(iter_previous)


      logical function __PROC(iter_equal)(this, other) result(eq)
         class (__iterator), intent(in) :: this
         class (__iterator), intent(in) :: other

         eq = (this%currentIndex == other%currentIndex)

      end function __PROC(iter_equal)


      logical function __PROC(not_iter_equal)(this, other) result(ne)
         class (__iterator), intent(in) :: this
         class (__iterator), intent(in) :: other

         ne = .not. (this == other)

      end function __PROC(not_iter_equal)


      ! Illegal to use these unless both arguments reference the
      ! same vector.
      logical function __PROC(iter_less)(this, other) result(less)
         class (__iterator), intent(in) :: this
         class (__iterator), intent(in) :: other
         less = (this%currentIndex < other%currentIndex)
      end function __PROC(iter_less)


      function __PROC(iter_less_equal)(this,other) result(le)
         logical :: le
         class (__iterator), intent(in) :: this
         class (__iterator), intent(in) :: other
         le = (this%currentIndex <= other%currentIndex)
      end function __PROC(iter_less_equal)

      ! =======================
      !  greaterThanIter
      ! =======================
      logical function __PROC(iter_greater)(this, other) result(gt)
         class (__iterator), intent(in) :: this
         class (__iterator), intent(in) :: other
         gt = (this%currentIndex > other%currentIndex)
      end function __PROC(iter_greater)

      function __PROC(iter_greater_equal)(this,other) result(gte)
         logical :: gte
         class (__iterator), intent(in) :: this
         class (__iterator), intent(in) :: other
         gte = (this%currentIndex >= other%currentIndex)
      end function __PROC(iter_greater_equal)


      function __PROC(iter_atDefault)(this) result(ptr)
         __type_declare_result, pointer :: ptr
         class (__iterator), intent(in) :: this

         ptr => __GET(this%elements(this%currentIndex))

      end function __PROC(iter_atDefault)


      function __PROC(iter_atOffset)(this, i) result(ptr)
         __type_declare_result, pointer :: ptr
         class (__iterator), intent(in) :: this
         integer(kind=SIZE_KIND), intent(in) :: i

         ptr => __GET(this%elements(this%currentIndex + i))

      end function __PROC(iter_atOffset)
#ifdef SUPPORT_FOR_INT64
      function __PROC(iter_atOffset_32)(this, i) result(ptr)
         __type_declare_result, pointer :: ptr
         class (__iterator), intent(in) :: this
         integer, intent(in) :: i

!!$         ptr => this%at(int(i,kind=SIZE_KIND))
         ! workaround for ifort 15.0.3 - no reproducer submitted
         ptr => __GET(this%elements(this%currentIndex + i))

      end function __PROC(iter_atOffset_32)
#endif

      function __PROC(iter_add)(this, n) result(newIter)
         type (__iterator) :: newIter
         class (__iterator), intent(in) :: this
         integer(kind=SIZE_KIND), intent(in) :: n

         newIter%currentIndex = this%currentIndex + n
         newIter%elements => this%elements

      end function __PROC(iter_add)

      function __PROC(iter_subtract)(this, n) result(newIter)
         type (__iterator) :: newIter
         class (__iterator), intent(in) :: this
         integer(kind=SIZE_KIND), intent(in) :: n

         newIter%currentIndex = this%currentIndex - n
         newIter%elements => this%elements

      end function __PROC(iter_subtract)

#ifdef SUPPORT_FOR_INT64
      function __PROC(iter_add_32)(this, n) result(newIter)
         type (__iterator) :: newIter
         class (__iterator), intent(in) :: this
         integer, intent(in) :: n

         newIter = this + int(n,kind=SIZE_KIND)

      end function __PROC(iter_add_32)

      function __PROC(iter_subtract_32)(this, n) result(newIter)
         type (__iterator) :: newIter
         class (__iterator), intent(in) :: this
         integer, intent(in) :: n

         newIter = this - int(n,kind=SIZE_KIND)

      end function __PROC(iter_subtract_32)
#endif


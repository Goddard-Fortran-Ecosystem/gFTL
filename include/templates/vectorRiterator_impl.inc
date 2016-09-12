
      function __PROC(riter_get)(this) result (item)
         class (__riterator), intent(in) :: this
         __type_declare_result, pointer :: item
         item => __GET(this%elements(this%currentIndex))

      end function __PROC(riter_get)


      subroutine __PROC(riter_next)(this)
         class (__riterator), intent(inout) :: this
         this%currentIndex = this%currentIndex - 1
      end subroutine __PROC(riter_next)


      subroutine __PROC(riter_previous)(this)
         class (__riterator), intent(inout) :: this
         this%currentIndex = this%currentIndex + 1
      end subroutine __PROC(riter_previous)


      logical function __PROC(riter_equal)(this, other) result(eq)
         class (__riterator), intent(in) :: this
         class (__riterator), intent(in) :: other

         eq = (this%currentIndex == other%currentIndex)

      end function __PROC(riter_equal)


      logical function __PROC(riter_not_equal)(this,other)result(neq)
         class (__riterator), intent(in) :: this
         class (__riterator), intent(in) :: other

         neq = .not. (this == other)

      end function __PROC(riter_not_equal)


      ! same vector.
      function __PROC(riter_less)(this, other) result(lt)
         logical :: lt
         class (__riterator), intent(in) :: this
         class (__riterator), intent(in) :: other
         lt = (this%currentIndex > other%currentIndex)
      end function __PROC(riter_less)


      function __PROC(riter_less_equal)(this, other) result (lte)
         logical :: lte
         class (__riterator), intent(in) :: this
         class (__riterator), intent(in) :: other
         lte = (this%currentIndex >= other%currentIndex)
      end function __PROC(riter_less_equal)


      function __PROC(riter_greater)(this, other) result(gt)
         logical :: gt
         class (__riterator), intent(in) :: this
         class (__riterator), intent(in) :: other
         gt = (this%currentIndex < other%currentIndex)
      end function __PROC(riter_greater)

      function __PROC(riter_greater_equal)(this, other) result(gte)
         logical :: gte
         class (__riterator), intent(in) :: this
         class (__riterator), intent(in) :: other
         gte = (this%currentIndex <= other%currentIndex)
      end function __PROC(riter_greater_equal)


      function __PROC(riter_atDefault)(this) result(ptr)
         __type_declare_result, pointer :: ptr
         class (__riterator), intent(in) :: this

         ptr => __GET(this%elements(this%currentIndex))

      end function __PROC(riter_atDefault)


      function __PROC(riter_atOffset)(this, i) result(ptr)
         __type_declare_result, pointer :: ptr
         class (__riterator), intent(in) :: this
         integer(kind=SIZE_KIND), intent(in) :: i

         ptr => __GET(this%elements(this%currentIndex - i))

      end function __PROC(riter_atOffset)
#ifdef SUPPORT_FOR_INT64
      function __PROC(riter_atOffset_32)(this, i) result(ptr)
         __type_declare_result, pointer :: ptr
         class (__riterator), intent(in) :: this
         integer, intent(in) :: i

         ptr => __GET(this%elements(this%currentIndex - i))

      end function __PROC(riter_atOffset_32)
#endif

      function __PROC(riter_add)(this, n) result(newIter)
         type (__riterator) :: newIter
         class (__riterator), intent(in) :: this
         integer(kind=SIZE_KIND), intent(in) :: n

         newIter%currentIndex = this%currentIndex - n
         newIter%elements => this%elements

      end function __PROC(riter_add)

      function __PROC(riter_subtract)(this, n) result(newIter)
         type (__riterator) :: newIter
         class (__riterator), intent(in) :: this
         integer(kind=SIZE_KIND), intent(in) :: n

         newIter%currentIndex = this%currentIndex + n
         newIter%elements => this%elements

      end function __PROC(riter_subtract)

#ifdef SUPPORT_FOR_INT64
      function __PROC(riter_add_32)(this, n) result(newIter)
         type (__riterator) :: newIter
         class (__riterator), intent(in) :: this
         integer, intent(in) :: n

         newIter = this + int(n, kind=SIZE_KIND)

      end function __PROC(riter_add_32)

      function __PROC(riter_subtract_32)(this, n) result(newIter)
         type (__riterator) :: newIter
         class (__riterator), intent(in) :: this
         integer, intent(in) :: n

         newIter = this - int(n, kind=SIZE_KIND)

      end function __PROC(riter_subtract_32)
#endif



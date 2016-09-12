#include "templates/all_template_macros.inc"

      function __PROC(newPair)(key, value) result(p)
         type (__pair) :: p
         __key_declare_dummy, intent(in) :: key
         __value_declare_dummy, intent(in) :: value

         __KEY_ASSIGN(p%key, key)
         __VALUE_ASSIGN(p%value, value)

      end function __PROC(newPair)

! =======================
!  pairEqual
! =======================
      function pairEqual(this, other) result(equal)
      class(__pair), intent(in) :: this, other
      logical :: equal

      equal = this%pairSameKey(other)

      end function pairEqual


! =======================
!  pairSameKey
! =======================
      function pairSameKey(this, other) result(sameKey)
      class (__pair), intent(in) :: this
      class (__pair), intent(in) :: other

      logical :: sameKey

      sameKey = .not. (__KEY_LESS_THAN(this%key, other%key)) .and.                   &
     &          .not. (__KEY_LESS_THAN(other%key, this%key))

   contains

! TODO:  possibly this procedure should be inside some sort of #ifdef
         logical function dictionaryLessThan1d(x, y) result(less)
            integer, intent(in) :: x(:)
            integer, intent(in) :: y(:)

            integer(kind=SIZE_KIND) :: i, n
            
            n = min(size(x),size(y))
            
            do i = 1, n
               less = (x(i) < y(i))
               if (.not. x(i) == y(i)) return
            end do
            
            less = (size(x) < size(y))
            
         end function dictionaryLessThan1d

#if defined(__key_needs_default_compare)
#  if !defined(__compare_type)
#    define __compare_declare_dummy __key_declare_dummy
#    define __compare_declare_component __key_declare_component
#    define __COMPARE_ASSIGN(x,y) __KEY_ASSIGN(x,y)
#    define __COMPARE_FREE(x) __KEY_FREE(x)
#  endif

         logical function defaultLessThan(x, y) result(less)
            __compare_declare_dummy, intent(in) :: x
            __compare_declare_dummy, intent(in) :: y

#  if  defined(_pointer)
            type LocalWrapper
               __compare_declare_component :: item
            end type LocalWrapper

            type (LocalWrapper) :: wrapX, wrapY
#    define __xx wrapX
#    define __yy wrapY

            __COMPARE_ASSIGN(wrapX%item, x)
            __COMPARE_ASSIGN(wrapY%item, y)

#  else

#    define __xx x
#    define __yy y

#  endif

            associate( wx => transfer(__xx,[1]),                                &
     &                 wy => transfer(__yy,[1]) )
              less = dictionaryLessThan1d(wx, wy)
            end associate

#  if  defined(_pointer)
            __COMPARE_FREE(wrapX%item)
            __COMPARE_FREE(wrapY%item)
#  endif
#  undef __xx
#  undef __yy
#  if !defined(__compare_type)
#    undef __compare_declare_dummy
#    undef __compare_declare_component
#    undef __COMPARE_ASSIGN
#    undef __COMPARE_FREE
#  endif
         end function defaultLessThan
#endif
      


      end function pairSameKey

#if !defined(_value_pointer) 
#  if (__value_rank > 0) & !defined(_value_extents)
#    define __array_declare_dummy __value_declare_dummy
#    define __ARRAY_EQ_ELEMENT(x,y) __VALUE_EQ_ELEMENT(x,y)
#    include "array_defs.inc"
#    define __already_did_it
#    undef __ARRAY_EQ_ELEMENT
#    undef __array_declare_dummy
#  endif
#endif

#if !defined(_key_pointer) 
#  if (__key_rank > 0) & !defined(_key_extents)
#    ifndef __already_did_it
#      define __array_declare_dummy __key_declare_dummy
#      define __ARRAY_EQ_ELEMENT(x,y) __KEY_EQ_ELEMENT(x,y)
#      include "array_defs.inc"
#      undef __ARRAY_EQ_ELEMENT
#      undef __array_declare_dummy
#    endif
#  endif
#endif

#undef __already_did_it

#include "templates/all_template_macros_undefs.inc"

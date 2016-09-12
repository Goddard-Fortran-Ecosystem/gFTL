include(header.m4)

module Test_`'param()VectorIterator_mod
#include "types/param().inc"
#include "type_test_values/param().inc"
   use pFUnit_mod, only: assertTrue, assertFalse
   use pFUnit_mod, only: TestSuite, newTestSuite
   use pFUnit_mod, only: newTestMethod
   use pFUnit_mod, only: SourceLocation
   use pFUnit_mod, only: anyExceptions
#ifdef _unlimited
   use pFUnitSupplement_mod, only: assertEqual
#else
   use pFUnit_mod, only: assertEqual
#endif
   use param()Vector_mod


#include "templates/type_set_use_tokens.inc"
#include "templates/type_template_macros.inc"
#include "templates/tmplbase.inc"
#include "templates/type_testing_macros.inc"

#include "genericItems_decl.inc"

   type (Vector) :: v

contains

#include <genericSetUpTearDown.inc>

@before
   subroutine setUp()
      call genericSetUp()

      v = Vector()
      call v%push_back(ONE)
      call v%push_back(TWO)
      call v%push_back(THREE)
      call v%push_back(FOUR)
      call v%push_back(FIVE)

   end subroutine setUp


@after
   subroutine tearDown()
      call v%clear()
      call genericTearDown()
   end subroutine tearDown

@test
   subroutine test_Begin()
      type (VectorIterator) :: iter

      iter = v%begin()
      @assertEqual(ONE, iter%get())
      
   end subroutine test_Begin

   ! Cannot test functionality of end() directly, so for now
   ! just ensure that the interface exists.
   ! The end() iterator cannot be dereferenced.
@test
   subroutine test_End()
      type (VectorIterator) :: iter

      iter = v%end()

   end subroutine test_End


@test
   subroutine test_Next()
      type (VectorIterator) :: iter

      iter = v%begin()
      call iter%next()
      @assertEqual(TWO, iter%get())
      call iter%next()
      @assertEqual(THREE, iter%get())
      
   end subroutine test_Next


@test
   subroutine test_Previous()
      type (VectorIterator) :: iter

      iter = v%begin()  ! ONE
      call iter%next()       ! TWO
      call iter%next()       ! THREE
      call iter%previous()   ! back to TWO
      @assertEqual(TWO, iter%get())
      call iter%previous()   ! back to ONE
      @assertEqual(ONE, iter%get())
      
   end subroutine test_Previous


   ! Check to make certain that get() can return a ptr that can be
   ! subsequently used.  Exposes error with ifort 15 for const length
   ! strings.
@test
   subroutine test_get_ptr()
      type (VectorIterator) :: iter
      __type_declare_result, pointer :: q

      iter = v%begin()
      q => iter%get()
      @assertTrue(associated(q))
      @assertEqual(ONE, q)
      
   end subroutine test_get_ptr


@test
   subroutine test_LessThan()
      type (VectorIterator) :: iter1, iter2

      iter1 = v%begin()
      iter2 = v%begin()
      @assertFalse(iter1 < iter2)

      call iter2%next()
      @assertTrue(iter1 < iter2)

      call iter1%next()
      call iter1%next()
      @assertFalse(iter1 < iter2)
      
   end subroutine test_LessThan


@test
   subroutine test_LessThanOrEqual()
      type (VectorIterator) :: iter1, iter2

      iter1 = v%begin()
      iter2 = v%begin()
      @assertTrue(iter1 <= iter2)

      call iter2%next()
      @assertTrue(iter1 <= iter2)

      call iter1%next()
      call iter1%next()
      @assertFalse(iter1 <= iter2)
      
   end subroutine test_LessThanOrEqual


@test
   subroutine test_GreaterThan()
      type (VectorIterator) :: iter1, iter2

      iter1 = v%begin()
      iter2 = v%begin()
      @assertFalse(iter1 > iter2)

      call iter2%next()
      @assertFalse(iter1 > iter2)

      call iter1%next()
      call iter1%next()
      @assertTrue(iter1 > iter2)
      
   end subroutine test_GreaterThan


@test
   subroutine test_GreaterThanOrEqual()
      type (VectorIterator) :: iter1, iter2

      iter1 = v%begin()
      iter2 = v%begin()
      @assertTrue(iter1 >= iter2)

      call iter2%next()
      @assertFalse(iter1 >= iter2)

      call iter1%next()
      call iter1%next()
      @assertTrue(iter1 >= iter2)
      
   end subroutine test_GreaterThanOrEqual


@test
   subroutine test_Equal()
      
      type (VectorIterator) :: iter1, iter2

      iter1 = v%begin()
      iter2 = v%begin()

      @assertTrue(iter1 == iter2)
      @assertFalse(iter1 /= iter2)

      call iter1%next()
      @assertFalse(iter1 == iter2)
      @assertTrue(iter1 /= iter2)

      call iter2%next()
      @assertTrue(iter1 == iter2)
      @assertFalse(iter1 /= iter2)

      call iter2%next()
      @assertFalse(iter1 == iter2)
      @assertTrue(iter1 /= iter2)

      call iter1%next()
      @assertTrue(iter1 == iter2)
      @assertFalse(iter1 /= iter2)

   end subroutine test_Equal

! This test is to show a more realistic use case
! for iterators.
@test
   subroutine test_IterationCount()
      
      type (VectorIterator) :: iter
      integer :: count

      count = 0
      iter = v%begin()
      do while (iter /= v%end())
         count = count + 1
         call iter%next()
      end do

      @assertEqual(v%size(), count)

   end subroutine test_IterationCount


@test
   subroutine test_ValidIteratorAfterVectorSwap()
      type (VectorIterator) :: i1, i2
      type (Vector) :: v1, v2

      v1 = Vector()
      call v1%push_back(TWO)
      call v1%push_back(THREE)
      call v1%push_back(FIVE)

      v2 = Vector()
      call v2%push_back(ONE)
      call v2%push_back(FOUR)
      call v2%push_back(ONE)

      i1 = v1%begin()
      i2 = v2%begin()

      call swap(v1, v2)
      @assertEqual(TWO, i1%get())

      call i1%next()
      @assertEqual(THREE, i1%get())

      ! Now check the other side of the swap
      @assertEqual(ONE, i2%get())

      call i2%next()
      @assertEqual(FOUR, i2%get())

   end subroutine test_ValidIteratorAfterVectorSwap


@test
   subroutine test_Rbegin()
      type (VectorRiterator) :: iter

      iter = v%rbegin()
      @assertEqual(FIVE, iter%get())
      
   end subroutine test_Rbegin


   ! Cannot test functionality of rEnd() directly, so for now
   ! just ensure that the interface exists.
   ! The rEnd() iterator cannot be dereferenced.
@test
   subroutine test_Rend()

      type (VectorRiterator) :: iter

      iter = v%rend()

   end subroutine test_Rend


@test
   subroutine test_Rnext()

      type (VectorRiterator) :: iter

      iter = v%rbegin()
      call iter%next()
      @assertEqual(FOUR, iter%get())
      call iter%next()
      @assertEqual(THREE, iter%get())
      
   end subroutine test_Rnext


@test
   subroutine test_Rprevious()

      type (VectorRiterator) :: iter

      iter = v%rbegin()  ! FIVE
      call iter%next()   ! FOUR
      call iter%next()   ! THREE
      call iter%previous() ! FOUR
      @assertEqual(FOUR, iter%get())
      
   end subroutine test_Rprevious


@test
   subroutine test_REqual()
      
      type (VectorRiterator) :: iter1, iter2

      iter1 = v%rbegin()
      iter2 = v%rbegin()

      @assertTrue(iter1 == iter2)
      @assertFalse(iter1 /= iter2)

   end subroutine test_REqual


@test
   subroutine test_RLessThan()

      type (VectorRiterator) :: iter1, iter2

      iter1 = v%rbegin()
      iter2 = v%rbegin()
      @assertFalse(iter1 < iter2)

      call iter2%next()
      @assertTrue(iter1 < iter2)

      call iter1%next()
      call iter1%next()
      @assertFalse(iter1 < iter2)
      
   end subroutine test_RLessThan

@test
   subroutine test_RLessThanOrEqual()

      type (VectorRiterator) :: iter1, iter2

      iter1 = v%rbegin()
      iter2 = v%rbegin()
      @assertTrue(iter1 <= iter2)

      call iter2%next()
      @assertTrue(iter1 <= iter2)

      call iter1%next()
      call iter1%next()
      @assertFalse(iter1 <= iter2)
      
   end subroutine test_RLessThanOrEqual


@test
   subroutine test_RGreaterThan()

      type (VectorRiterator) :: iter1, iter2

      iter1 = v%rbegin()
      iter2 = v%rbegin()
      @assertFalse(iter1 > iter2)

      call iter2%next()
      @assertFalse(iter1 > iter2)

      call iter1%next()
      call iter1%next()
      @assertTrue(iter1 > iter2)
      
   end subroutine test_RGreaterThan

@test
   subroutine test_RGreaterThanOrEqual()

      type (VectorRiterator) :: iter1, iter2

      iter1 = v%rbegin()
      iter2 = v%rbegin()
      @assertTrue(iter1 >= iter2)

      call iter2%next()
      @assertFalse(iter1 >= iter2)

      call iter1%next()
      call iter1%next()
      @assertTrue(iter1 >= iter2)
      
   end subroutine test_RGreaterThanOrEqual


@test
   subroutine test_ValidRIteratorAfterVectorSwap()

      type (VectorRiterator) :: i1, i2
      type (Vector) :: v1, v2

      v1 = Vector()
      call v1%push_back(TWO)
      call v1%push_back(THREE)
      call v1%push_back(FIVE)

      v2 = Vector()
      call v2%push_back(ONE)
      call v2%push_back(FOUR)
      call v2%push_back(ONE)

      i1 = v1%rBegin()
      i2 = v2%rBegin()

      call swap(v1, v2)
      @assertEqual(FIVE, i1%get())

      call i1%next()
      @assertEqual(THREE, i1%get())

      ! Now check the other side of the swap
      call swap(v1, v2)
      @assertEqual(ONE, i2%get())

      call i2%next()
      @assertEqual(FOUR, i2%get())

   end subroutine test_ValidRIteratorAfterVectorSwap

! This test is to show a more realistic use case
! for iterators.
@test
   subroutine test_rIterationCount()
      
      type (VectorRiterator) :: iter
      integer :: count

      count = 0
      iter = v%rbegin()
      do while (iter /= v%rend())
         count = count + 1
         call iter%next
      end do

      @assertEqual(v%size(), count)

   end subroutine test_rIterationCount


@test
   subroutine test_At()

      type (VectorIterator) :: iter

      iter = v%begin()
      call iter%next()

      @assertEqual(TWO, iter%at(  ))
      @assertEqual(TWO, iter%at( 0))
      @assertEqual(ONE, iter%at(-1))
      @assertEqual(THREE, iter%at(+1))
      
   end subroutine test_At


@test
   subroutine test_Rat()

      type (VectorRiterator) :: iter

      iter = v%rbegin()
      call iter%next()

      @assertEqual(FOUR, iter%at( ))
      @assertEqual(FOUR, iter%at( 0))
      @assertEqual(FIVE, iter%at(-1))
      
   end subroutine test_Rat

@test
   subroutine test_Add()

      type (VectorIterator) :: iter

      iter = v%begin() + 2
      @assertEqual(THREE, iter%at())

   end subroutine test_Add


@test
   subroutine test_rAdd()
      type (VectorRiterator) :: iter

      iter = v%rbegin() + 2
      @assertEqual(THREE, iter%at())

   end subroutine test_rAdd

#include "templates/type_use_tokens_undef.inc"

end module Test_`'param()VectorIterator_mod
#include "templates/tmpltail.inc"

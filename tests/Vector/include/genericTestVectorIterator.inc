
#define CHECK if(anyExceptions()) return
#define _FILE 'genericTestVectorIterator.inc'

#define ASSERT_EQUAL(a,b)call assertEqual(a,b,location=SourceLocation(_FILE,__LINE__)); CHECK

#define ASSERT_TRUE(a)call assertTrue(a,location=SourceLocation(_FILE,__LINE__)); CHECK
#define ASSERT_FALSE(a)call assertFalse(a,location=SourceLocation(_FILE,__LINE__)); CHECK

#include <templates/type_template_macros.inc>
#include <templates/testing_macros.inc>
#include <genericItems_decl.inc>

   type (Vector) :: v

contains

#include <genericSetUpTearDown.inc>

   subroutine setUp()
      call genericSetUp()

      v = Vector()
      call v%push_back(ONE)
      call v%push_back(TWO)
      call v%push_back(THREE)
      call v%push_back(FOUR)
      call v%push_back(FIVE)


   end subroutine setUp

   subroutine tearDown()
      call v%clear()
      call genericTearDown()
   end subroutine tearDown


   subroutine test_Begin()
      type (VectorIterator) :: iter

      iter = v%begin()
      ASSERT_EQUAL(ONE, iter%get())
      
   end subroutine test_Begin

   ! Cannot test functionality of end() directly, so for now
   ! just ensure that the interface exists.
   ! The end() iterator cannot be dereferenced.
   subroutine test_End()
      type (VectorIterator) :: iter

      iter = v%end()

   end subroutine test_End


   subroutine test_Next()
      type (VectorIterator) :: iter

      iter = v%begin()
      call iter%next()
      ASSERT_EQUAL(TWO, iter%get())
      call iter%next()
      ASSERT_EQUAL(THREE, iter%get())
      
   end subroutine test_Next


   subroutine test_Previous()
      type (VectorIterator) :: iter

      iter = v%begin()  ! ONE
      call iter%next()       ! TWO
      call iter%next()       ! THREE
      call iter%previous()   ! back to TWO
      ASSERT_EQUAL(TWO, iter%get())
      call iter%previous()   ! back to ONE
      ASSERT_EQUAL(ONE, iter%get())
      
   end subroutine test_Previous


   ! Check to make certain that get() can return a ptr that can be
   ! subsequently used.  Exposes error with ifort 15 for const length
   ! strings.
   subroutine test_get_ptr()
      type (VectorIterator) :: iter
      __type_declare_result, pointer :: q

      iter = v%begin()
      q => iter%get()
      ASSERT_TRUE(associated(q))
      ASSERT_EQUAL(ONE, q)
      
   end subroutine test_get_ptr


   subroutine test_LessThan()
      type (VectorIterator) :: iter1, iter2

      iter1 = v%begin()
      iter2 = v%begin()
      ASSERT_FALSE(iter1 < iter2)

      call iter2%next()
      ASSERT_TRUE(iter1 < iter2)

      call iter1%next()
      call iter1%next()
      ASSERT_FALSE(iter1 < iter2)
      
   end subroutine test_LessThan

   subroutine test_LessThanOrEqual()
      type (VectorIterator) :: iter1, iter2

      iter1 = v%begin()
      iter2 = v%begin()
      ASSERT_TRUE(iter1 <= iter2)

      call iter2%next()
      ASSERT_TRUE(iter1 <= iter2)

      call iter1%next()
      call iter1%next()
      ASSERT_FALSE(iter1 <= iter2)
      
   end subroutine test_LessThanOrEqual


   subroutine test_GreaterThan()
      type (VectorIterator) :: iter1, iter2

      iter1 = v%begin()
      iter2 = v%begin()
      ASSERT_FALSE(iter1 > iter2)

      call iter2%next()
      ASSERT_FALSE(iter1 > iter2)

      call iter1%next()
      call iter1%next()
      ASSERT_TRUE(iter1 > iter2)
      
   end subroutine test_GreaterThan

   subroutine test_GreaterThanOrEqual()
      type (VectorIterator) :: iter1, iter2

      iter1 = v%begin()
      iter2 = v%begin()
      ASSERT_TRUE(iter1 >= iter2)

      call iter2%next()
      ASSERT_FALSE(iter1 >= iter2)

      call iter1%next()
      call iter1%next()
      ASSERT_TRUE(iter1 >= iter2)
      
   end subroutine test_GreaterThanOrEqual


   subroutine test_Equal()
      
      type (VectorIterator) :: iter1, iter2

      iter1 = v%begin()
      iter2 = v%begin()

      ASSERT_TRUE(iter1 == iter2)
      ASSERT_FALSE(iter1 /= iter2)

      call iter1%next()
      ASSERT_FALSE(iter1 == iter2)
      ASSERT_TRUE(iter1 /= iter2)

      call iter2%next()
      ASSERT_TRUE(iter1 == iter2)
      ASSERT_FALSE(iter1 /= iter2)

      call iter2%next()
      ASSERT_FALSE(iter1 == iter2)
      ASSERT_TRUE(iter1 /= iter2)

      call iter1%next()
      ASSERT_TRUE(iter1 == iter2)
      ASSERT_FALSE(iter1 /= iter2)

   end subroutine test_Equal

! This test is to show a more realistic use case
! for iterators.
   subroutine test_IterationCount()
      
      type (VectorIterator) :: iter
      integer :: count

      count = 0
      iter = v%begin()
      do while (iter /= v%end())
         count = count + 1
         call iter%next
      end do

      ASSERT_EQUAL(v%size(), count)

   end subroutine test_IterationCount


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
      ASSERT_EQUAL(TWO, i1%get())

      call i1%next()
      ASSERT_EQUAL(THREE, i1%get())

      ! Now check the other side of the swap
      ASSERT_EQUAL(ONE, i2%get())

      call i2%next()
      ASSERT_EQUAL(FOUR, i2%get())

   end subroutine test_ValidIteratorAfterVectorSwap



   subroutine test_Rbegin()
      type (VectorRiterator) :: iter

      iter = v%rbegin()
      ASSERT_EQUAL(FIVE, iter%get())
      
   end subroutine test_Rbegin



   ! Cannot test functionality of rEnd() directly, so for now
   ! just ensure that the interface exists.
   ! The rEnd() iterator cannot be dereferenced.
   subroutine test_Rend()

      type (VectorRiterator) :: iter

      iter = v%rend()

   end subroutine test_Rend


   subroutine test_Rnext()

      type (VectorRiterator) :: iter

      iter = v%rbegin()
      call iter%next()
      ASSERT_EQUAL(FOUR, iter%get())
      call iter%next()
      ASSERT_EQUAL(THREE, iter%get())
      
   end subroutine test_Rnext

   subroutine test_Rprevious()

      type (VectorRiterator) :: iter

      iter = v%rbegin()  ! FIVE
      call iter%next()   ! FOUR
      call iter%next()   ! THREE
      call iter%previous() ! FOUR
      ASSERT_EQUAL(FOUR, iter%get())
      
   end subroutine test_Rprevious


   subroutine test_REqual()
      
      type (VectorRiterator) :: iter1, iter2

      iter1 = v%rbegin()
      iter2 = v%rbegin()

      ASSERT_TRUE(iter1 == iter2)
      ASSERT_FALSE(iter1 /= iter2)

   end subroutine test_REqual


   subroutine test_RLessThan()

      type (VectorRiterator) :: iter1, iter2

      iter1 = v%rbegin()
      iter2 = v%rbegin()
      ASSERT_FALSE(iter1 < iter2)

      call iter2%next()
      ASSERT_TRUE(iter1 < iter2)

      call iter1%next()
      call iter1%next()
      ASSERT_FALSE(iter1 < iter2)
      
   end subroutine test_RLessThan

   subroutine test_RLessThanOrEqual()

      type (VectorRiterator) :: iter1, iter2

      iter1 = v%rbegin()
      iter2 = v%rbegin()
      ASSERT_TRUE(iter1 <= iter2)

      call iter2%next()
      ASSERT_TRUE(iter1 <= iter2)

      call iter1%next()
      call iter1%next()
      ASSERT_FALSE(iter1 <= iter2)
      
   end subroutine test_RLessThanOrEqual


   subroutine test_RGreaterThan()

      type (VectorRiterator) :: iter1, iter2

      iter1 = v%rbegin()
      iter2 = v%rbegin()
      ASSERT_FALSE(iter1 > iter2)

      call iter2%next()
      ASSERT_FALSE(iter1 > iter2)

      call iter1%next()
      call iter1%next()
      ASSERT_TRUE(iter1 > iter2)
      
   end subroutine test_RGreaterThan

   subroutine test_RGreaterThanOrEqual()

      type (VectorRiterator) :: iter1, iter2

      iter1 = v%rbegin()
      iter2 = v%rbegin()
      ASSERT_TRUE(iter1 >= iter2)

      call iter2%next()
      ASSERT_FALSE(iter1 >= iter2)

      call iter1%next()
      call iter1%next()
      ASSERT_TRUE(iter1 >= iter2)
      
   end subroutine test_RGreaterThanOrEqual


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
      ASSERT_EQUAL(FIVE, i1%get())

      call i1%next()
      ASSERT_EQUAL(THREE, i1%get())

      ! Now check the other side of the swap
      call swap(v1, v2)
      ASSERT_EQUAL(ONE, i2%get())

      call i2%next()
      ASSERT_EQUAL(FOUR, i2%get())

   end subroutine test_ValidRIteratorAfterVectorSwap

! This test is to show a more realistic use case
! for iterators.
   subroutine test_rIterationCount()
      
      type (VectorRiterator) :: iter
      integer :: count

      count = 0
      iter = v%rbegin()
      do while (iter /= v%rend())
         count = count + 1
         call iter%next
      end do

      ASSERT_EQUAL(v%size(), count)

   end subroutine test_rIterationCount



   subroutine test_At()

      type (VectorIterator) :: iter

      iter = v%begin()
      call iter%next()

      ASSERT_EQUAL(TWO, iter%at(  ))
      ASSERT_EQUAL(TWO, iter%at( 0))
      ASSERT_EQUAL(ONE, iter%at(-1))
      ASSERT_EQUAL(THREE, iter%at(+1))
      
   end subroutine test_At


   subroutine test_Rat()

      type (VectorRiterator) :: iter

      iter = v%rbegin()
      call iter%next()

      ASSERT_EQUAL(FOUR, iter%at( ))
      ASSERT_EQUAL(FOUR, iter%at( 0))
      ASSERT_EQUAL(FIVE, iter%at(-1))
      
   end subroutine test_Rat

   subroutine test_Add()

      type (VectorIterator) :: iter

      iter = v%begin() + 2
      ASSERT_EQUAL(THREE, iter%at())

   end subroutine test_Add

   subroutine test_rAdd()
      type (VectorRiterator) :: iter

      iter = v%rbegin() + 2
      ASSERT_EQUAL(THREE, iter%at())

   end subroutine test_rAdd


   function _suite() result(s)
      type (TestSuite) :: s

      s = newTestSuite(_suite_name)

      call addForwardIteratorTests()
      call addReverseIteratorTests()

   contains
      
      subroutine addForwardIteratorTests()
         call s%addTest(newTestMethod('test_Begin', test_Begin, setUp, tearDown))
         call s%addTest(newTestMethod('test_End', test_End, setUp, tearDown))
         call s%addTest(newTestMethod('test_Next', test_Next, setUp, tearDown))
         call s%addTest(newTestMethod('test_Previous', test_Previous, setUp, tearDown))
         call s%addTest(newTestMethod('test_get_ptr', test_get_ptr, setUp, tearDown))
         
         ! Relational tests
         call s%addTest(newTestMethod('test_LessThan', test_LessThan, setUp, tearDown))
         call s%addTest(newTestMethod('test_LessThanOrEqual', test_LessThanOrEqual, &
              & setUp, tearDown))
         call s%addTest(newTestMethod('test_GreaterThan', test_GreaterThan, setUp, tearDown))
         call s%addTest(newTestMethod('test_GreaterThanOrEqual', test_GreaterThanOrEqual, &
              & setUp, tearDown))
         call s%addTest(newTestMethod('test_Equal', test_Equal, setUp, tearDown))
         
         call s%addTest(newTestMethod('test_IterationCount', test_IterationCount, &
              & setUp, tearDown))
         
         call s%addTest(newTestMethod('test_ValidIteratorAfterVectorSwap', &
              & test_ValidIteratorAfterVectorSwap, &
              & setUp, tearDown))

         call s%addTest(newTestMethod('test_at', test_at, setUp, tearDown))
         call s%addTest(newTestMethod('test_add', test_add, setUp, tearDown))

      end subroutine addForwardIteratorTests

      subroutine addReverseIteratorTests()
         call s%addTest(newTestMethod('test_rBegin', test_rBegin, setUp, tearDown))
         call s%addTest(newTestMethod('test_rEnd', test_rEnd, setUp, tearDown))
         call s%addTest(newTestMethod('test_rNext', test_rNext, setUp, tearDown))
         call s%addTest(newTestMethod('test_rPrevious', test_rPrevious, setUp, tearDown))
         
         ! Relational tests
         call s%addTest(newTestMethod('test_rLessThan', test_rLessThan, setUp, tearDown))
         call s%addTest(newTestMethod('test_rLessThanOrEqual', test_rLessThanOrEqual, &
              & setUp, tearDown))
         call s%addTest(newTestMethod('test_rGreaterThan', test_rGreaterThan, setUp, tearDown))
         call s%addTest(newTestMethod('test_rGreaterThanOrEqual', test_rGreaterThanOrEqual, &
              & setUp, tearDown))
         call s%addTest(newTestMethod('test_rEqual', test_rEqual, setUp, tearDown))
         
         call s%addTest(newTestMethod('test_rIterationCount', test_rIterationCount, &
              & setUp, tearDown))
         
         call s%addTest(newTestMethod('test_ValidRIteratorAfterVectorSwap', &
              & test_ValidRIteratorAfterVectorSwap, &
              & setUp, tearDown))

         call s%addTest(newTestMethod('test_rat', test_rat, setUp, tearDown))
         call s%addTest(newTestMethod('test_radd', test_radd, setUp, tearDown))

      end subroutine addReverseIteratorTests

   end function _suite



#define CHECK if(anyExceptions()) return
#define _FILE 'genericTestVector.inc'

#define ASSERT_EQUAL(a,b)call assertEqual(a,b,location=SourceLocation(_FILE,__LINE__)); CHECK

#define ASSERT_TRUE(a)call assertTrue(a,location=SourceLocation(_FILE,__LINE__)); CHECK
#define ASSERT_FALSE(a)call assertFalse(a,location=SourceLocation(_FILE,__LINE__)); CHECK

#include <tmplhead.inc>

#ifdef _wrapentry
#define _local _wrapentry
#else
#define _local _entry, pointer
#endif

#ifdef _wrapentry
   _wrapentry :: ONE
   _wrapentry :: TWO
   _wrapentry :: THREE
   _wrapentry :: FOUR
   _wrapentry :: FIVE
#else
   _entry :: ONE
   _entry :: TWO
   _entry :: THREE
   _entry :: FOUR
   _entry :: FIVE
#endif

#ifdef _logical
   interface assertEqual
      module procedure assertEqual_logical
   end interface assertEqual
#endif

contains

#ifdef _logical
   subroutine assertEqual_logical(a, b, message, location)
      logical, intent(in) :: a
      logical, intent(in) :: b
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      call assertTrue(a .eqv. b, message=message, location=location)
   end subroutine assertEqual_logical
#endif

   subroutine setUp()
#ifdef _pointer
      allocate(ONE, source=_ONE)
      allocate(TWO, source=_TWO)
      allocate(THREE, source=_THREE)
      allocate(FOUR, source=_FOUR)
      allocate(FIVE, source=_FIVE)
#else
      ONE = _ONE
      two = _TWO
      three = _THREE
      four = _FOUR
      five = _FIVE
#endif
   end subroutine setUp

   subroutine tearDown()
#ifdef _pointer
      deallocate(ONE)
      deallocate(TWO)
      deallocate(THREE)
      deallocate(FOUR)
      deallocate(FIVE)
#endif
   end subroutine tearDown

      
   subroutine testSizeEmpty()
      type (Vector) :: v

       v = Vector()
       ASSERT_EQUAL(0, v%size())

   end subroutine testSizeEmpty


   subroutine testEmpty()
      type (Vector) :: v

      v = Vector()
      ASSERT_TRUE(v%empty())

   end subroutine testEmpty

#ifndef _wrapentry
   subroutine testCopyFromArray_notEmpty()
      type (Vector) :: v

      v = [ONE]
      ASSERT_FALSE(v%empty())

   end subroutine testCopyFromArray_notEmpty
#endif

#ifndef _wrapentry
   subroutine testCopyFromArray_size()
      type (Vector) :: v

      v = [ONE,TWO]
      ASSERT_EQUAL(2, v%size())

   end subroutine testCopyFromArray_size
#endif



   subroutine test_push_back()
      type (Vector) :: v
      _retentry, pointer :: q

      v = Vector()
      call v%push_back(ONE)
      ASSERT_EQUAL(1, v%size())

      q => v%at(1)
      ASSERT_EQUAL(ONE, q)

      call v%push_back(TWO)
      ASSERT_EQUAL(2, v%size())
      q => v%at(2)
      ASSERT_EQUAL(TWO, q)

   end subroutine test_push_back


   function SUITE() result(s)
      type (TestSuite) :: s

      s = newTestSuite(SUITE_NAME)

      call s%addTest(newTestMethod('testSizeEmpty', testSizeEmpty, &
           & setUp, tearDown))
      call s%addTest(newTestMethod('testEmpty', testSizeEmpty, &
           & setUp, tearDown))

#ifndef _wrapentry
      call s%addTest(newTestMethod('testCopyFromArray_notEmpty', testCopyFromArray_notEmpty, &
           & setUp, tearDown))
      call s%addTest(newTestMethod('testCopyFromArray_size', testCopyFromArray_size, &
           & setUp, tearDown))
#endif

      call s%addTest(newTestMethod('test_push_back', test_push_back, &
           & setUp, tearDown))

   end function SUITE


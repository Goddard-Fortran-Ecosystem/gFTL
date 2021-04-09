
#define CHECK if(anyExceptions()) return
#define _FILE 'genericTestMap.inc'

#define ASSERT_EQUAL(a,b)call assertEqual(a,b,location=SourceLocation(_FILE,__LINE__)); CHECK

#define ASSERT_TRUE(a)call assertTrue(a,location=SourceLocation(_FILE,__LINE__)); CHECK
#define ASSERT_FALSE(a)call assertFalse(a,location=SourceLocation(_FILE,__LINE__)); CHECK

#include <templates/type_template_macros.inc>
#include <templates/testing_macros.inc>
#include <genericItems_decl.inc>

contains

#include "genericSetUpTearDown.inc"

   subroutine setUp()
      call genericSetUp()
   end subroutine setUp

   subroutine tearDown()
      call genericTearDown()
   end subroutine tearDown
      

   subroutine testIsSet()
      type (Map) :: m
      logical :: f
      integer, pointer :: val

      call m%set(KEY1,ONE)
      f = m%get(KEY1, val)
      ASSERT_TRUE(f)

   end subroutine testIsSet


   subroutine testNotSet()
      type (Map) :: m
      logical :: f
      integer, pointer :: val

      call m%set(KEY1,ONE)
      f = m%get(KEY2, val)
      ASSERT_FALSE(f)

   end subroutine testNotSet


   subroutine testGet()
      type (Map) :: m
      logical :: f
      integer, pointer :: val

      call m%set(KEY1,ONE)
      call m%set(KEY2,TWO)

      f = m%get(KEY1, val)
      ASSERT_TRUE(f)
      ASSERT_EQUAL(ONE, val)

      f = m%get(KEY2, val)
      ASSERT_TRUE(f)
      ASSERT_EQUAL(TWO, val)

   end subroutine testGet


   function _suite() result(s)
      type (TestSuite) :: s

      s = newTestSuite(_suite_name)

      call s%addTest(newTestMethod('testIsSet', testIsSet, &
           & setUp, tearDown))
      call s%addTest(newTestMethod('testNotSet', testNotSet, &
           & setUp, tearDown))
      call s%addTest(newTestMethod('testGet', testGet, &
           & setUp, tearDown))

   end function _suite


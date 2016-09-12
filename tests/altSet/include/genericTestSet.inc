
#define CHECK if(anyExceptions()) return
#define _FILE 'genericTestSet.inc'

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
      

   subroutine testIsEmpty()
      type (Set) :: s

      ASSERT_TRUE(s%empty())

   end subroutine testIsEmpty


   subroutine testIsEmpty_notEmpty()
      type (Set) :: s

      call s%insert(ONE)
      ASSERT_FALSE(s%empty())

   end subroutine testIsEmpty_notEmpty


   subroutine testSize_empty()
      type (Set) :: s

      ASSERT_EQUAL(0, s%size())

   end subroutine testSize_empty


   subroutine testSize_simple()
      type (Set) :: s

      call s%insert(ONE)
      ASSERT_EQUAL(1, s%size())
      call s%insert(TWO)
      ASSERT_EQUAL(2, s%size())

   end subroutine testSize_simple


   subroutine testSize_duplicate()
      type (Set) :: s

      call s%insert(ONE)
      ASSERT_EQUAL(1, s%size())
      call s%insert(TWO)
      ASSERT_EQUAL(2, s%size())

      ! Insert a duplicate entry - should not modify
      call s%insert(ONE)
      ASSERT_EQUAL(2, s%size())

   end subroutine testSize_duplicate


   subroutine testInsert_isNew()
      type (Set) :: s
      logical :: isNew

      call s%insert(ONE,isNew=isNew)
      ASSERT_TRUE(isNew)

      call s%insert(TWO, isNew=isNew)
      ASSERT_TRUE(isNew)

      call s%insert(ONE, isNew=isNew)
      ASSERT_FALSE(isNew)

      call s%insert(TWO, isNew=isNew)
      ASSERT_FALSE(isNew)

   end subroutine testInsert_isNew

   subroutine testCount()
      type (Set) :: s

      ASSERT_EQUAL(0, s%count(ONE))
      ASSERT_EQUAL(0, s%count(TWO))

      call s%insert(ONE)
      ASSERT_EQUAL(1, s%count(ONE))
      ASSERT_EQUAL(0, s%count(TWO))

      call s%insert(TWO)
      ASSERT_EQUAL(1, s%count(ONE))
      ASSERT_EQUAL(1, s%count(TWO))

      ! duplicate
      call s%insert(TWO)
      ASSERT_EQUAL(1, s%count(ONE))
      ASSERT_EQUAL(1, s%count(TWO))

   end subroutine testCount

#ifdef _pointer
! This test verifies that if two pointers are put in a Set
! they are treated as separate entries even if their targets
! have the same value.  (But the targets are not the same object.)

! This test is only relevant for types with _pointer.
   subroutine test_findSameTarget()
      type (Set) :: s

#  ifdef __allocatable_target
#    define __test_attrs , allocatable
#  else
#   define __test_attrs
#  endif

#define _ONE_  _ONE
      __declare_local :: pA
      __declare_local :: pB
      __declare_local :: pC
      __type_declare_target __test_attrs :: targA
      __type_declare_target __test_attrs :: targB
      __type_declare_target __test_attrs :: targC

      __type_declare_result, pointer :: qA, qB, qC

      type (Iterator) :: iterA, iterB, iterC
      logical :: isNew

      __INIT(pA, _ONE, targA)
      __INIT(pB, _ONE, targB)
      __INIT(pC, _ONE, targC)
      ASSERT_FALSE(associated(pA, pB))

      call s%insert(pA)
      ASSERT_EQUAL(1, s%size())

      call s%insert(pB, isNew=isNew)
      ASSERT_TRUE(isNew)
      ASSERT_EQUAL(2, s%size())

      call s%insert(pC, isNew=isNew)
      ASSERT_TRUE(isNew)
      ASSERT_EQUAL(3, s%size())

      iterA = s%find(pA)
      iterB = s%find(pB)
      iterC = s%find(pC)

      qA => iterA%value()
      qB => iterB%value()
      qC => iterC%value()
      ASSERT_FALSE(associated(qA, qB))
      ASSERT_FALSE(associated(qA, qC))
      ASSERT_FALSE(associated(qB, qC))


   end subroutine test_findSameTarget
#endif


   subroutine test_eraseOne()
      type (Set), target :: s
      type (Iterator) :: iter

      call s%insert(ONE)
      call s%insert(THREE)
      call s%insert(FIVE)

      iter = s%find(THREE)
      call s%erase(iter)

      ASSERT_EQUAL(2, s%size())
      ASSERT_EQUAL(0, s%count(THREE))

      ASSERT_EQUAL(1, s%count(ONE))
      ASSERT_EQUAL(1, s%count(FIVE))

   end subroutine test_eraseOne

! In the case of containers of pointers, it is very difficult to know what
! is included in a range.  Thus we copy the set and use it as a reference.
   subroutine test_eraseRange()
      type (Set) :: s
      type (Set) :: sCopy
      type (Iterator) :: first
      type (Iterator) :: last
      type (Iterator) :: iter

      __type_declare_result, pointer :: q

      call s%insert(ONE)
      call s%insert(THREE)
      call s%insert(FOUR)
      call s%insert(FIVE)

      call sCopy%insert(ONE)
      call sCopy%insert(THREE)
      call sCopy%insert(FOUR)
      call sCopy%insert(FIVE)

      first = s%begin()
      call first%next()
      last = s%end()
      call last%prev()

      call s%erase(first, last)
      ASSERT_TRUE(first == last)

      ASSERT_EQUAL(2, s%size())

      iter = sCopy%begin()
      call iter%next()
      last = sCopy%end()
      call last%prev()

      do while (iter /= last)
         q => iter%value()
         ASSERT_EQUAL(0, s%count(q))
         call iter%next()
      end do

   end subroutine test_eraseRange
   
   subroutine test_eraseAll()
      type (Set) :: s
      type (Iterator) :: first
      type (Iterator) :: last

      __type_declare_result, pointer :: q

      call s%insert(ONE)
      call s%insert(THREE)
      call s%insert(FIVE)

      first = s%begin()
      last = s%end()

      call s%erase(first, last)

      ASSERT_EQUAL(0, s%size())

      ! Iterator should now point to end of updated set.
      ASSERT_TRUE(first == s%end())

   end subroutine test_eraseAll


   subroutine test_equalEmpty()
      type (Set) :: a, b

      ASSERT_TRUE(a == b)
      ASSERT_FALSE(a /= b)

   end subroutine test_equalEmpty


   subroutine test_equal()
      type (Set), target :: a, b

      call a%insert(ONE)
      call a%insert(TWO)

      call b%insert(ONE)
      call b%insert(TWO)

      ASSERT_TRUE(a == b)
      ASSERT_FALSE(a /= b)

   end subroutine test_equal


   subroutine test_notEqual()
      type (Set) :: a, b

      call a%insert(ONE)
      call a%insert(TWO)
      call a%insert(FOUR)

      call b%insert(ONE)
      call b%insert(TWO)
      call b%insert(FIVE)

      ASSERT_FALSE(a == b)
      ASSERT_TRUE(a /= b)

   end subroutine test_notEqual


   subroutine test_deepCopy()
      type (Set) :: a, b

      call a%insert(ONE)
      call a%insert(TWO)

      b = a
      ASSERT_TRUE(a == b)

      ! Shallow copy will show problems if we now insert an element
      ! and compare again.
      call b%insert(THREE)
      ASSERT_TRUE(a /= b)

   end subroutine test_deepCopy

   ! Ensure that deep copy obliterates any state the variable on the
   ! LHS had prior to the assignment.
   subroutine test_deepCopy2()
      type (Set) :: a, b

      call a%insert(ONE)
      call a%insert(TWO)
      
      call b%insert(THREE)

      b = a
      ASSERT_TRUE(a == b)

      ! Shallow copy will show problems if we now insert an element
      ! and compare again.
      call b%insert(THREE)
      ASSERT_TRUE(a /= b)

   end subroutine test_deepCopy2



   function _suite() result(s)
      type (TestSuite) :: s

      s = newTestSuite(_suite_name)

      call add('testIsEmpty', testIsEmpty)
      call add('testIsEmpty_notEmpty', testIsEmpty_notEmpty)
      call add('testSize_empty', testSize_empty)
      call add('testSize_simple', testSize_simple)
      call add('testSize_duplicate', testSize_duplicate)
      call add('testInsert_isNew', testInsert_isNew)
      call add('testCount', testCount)
#ifdef _pointer
      call add('test_findSameTarget', test_findSameTarget)
#endif

      call add('test_eraseOne', test_eraseOne)
      call add('test_eraseRange', test_eraseRange)
      call add('test_eraseAll', test_eraseAll)

      call add('test_equalEmpty', test_equalEmpty)
      call add('test_equal', test_equal)
      call add('test_notEqual', test_notEqual)

      call add('test_deepCopy', test_deepCopy)
      call add('test_deepCopy2', test_deepCopy2)

   contains

      subroutine add(name, proc)
         character(len=*), intent(in) :: name
         interface
            subroutine proc()
            end subroutine proc
         end interface

         call s%addTest(newTestMethod(name, proc, setUp, tearDown))
         
      end subroutine add

   end function _suite


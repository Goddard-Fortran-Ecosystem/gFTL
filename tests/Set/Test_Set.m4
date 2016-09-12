include(header.m4)

module Test_`'param()Set_mod
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
   use param()Set_mod


#include "templates/type_set_use_tokens.inc"
#include "templates/type_template_macros.inc"
#include "templates/tmplbase.inc"
#include "templates/type_testing_macros.inc"

#include "genericItems_decl.inc"

contains

#include "genericSetUpTearDown.inc"
@before
   subroutine setUp()
      call genericSetUp()
   end subroutine setUp


@after
   subroutine tearDown()
      call genericTearDown()
   end subroutine tearDown


@test
   subroutine testIsEmpty()
      type (Set) :: s

      @assertTrue(s%empty())

   end subroutine testIsEmpty


@test
   subroutine testIsEmpty_notEmpty()
      type (Set) :: s

      call s%insert(ONE)
      @assertFalse(s%empty())

   end subroutine testIsEmpty_notEmpty


@test
   subroutine testSize_empty()
      type (Set) :: s

      @assertEqual(0, s%size())

   end subroutine testSize_empty


@test
   subroutine testSize_simple()
      type (Set) :: s

      call s%insert(ONE)
      @assertEqual(1, s%size())
      call s%insert(TWO)
      @assertEqual(2, s%size())

   end subroutine testSize_simple


@test
   subroutine testSize_duplicate()
      type (Set) :: s

      call s%insert(ONE)
      @assertEqual(1, s%size())
      call s%insert(TWO)
      @assertEqual(2, s%size())

      ! Insert a duplicate entry - should not modify
      call s%insert(ONE)
      @assertEqual(2, s%size())

   end subroutine testSize_duplicate


@test
   subroutine testInsert_isNew()
      type (Set) :: s
      logical :: isNew

      call s%insert(ONE,isNew=isNew)
      @assertTrue(isNew)

      call s%insert(TWO, isNew=isNew)
      @assertTrue(isNew)

      call s%insert(ONE, isNew=isNew)
      @assertFalse(isNew)

      call s%insert(TWO, isNew=isNew)
      @assertFalse(isNew)

   end subroutine testInsert_isNew


@test
   subroutine testCount()
      type (Set) :: s

      @assertEqual(0, s%count(ONE))
      @assertEqual(0, s%count(TWO))

      call s%insert(ONE)
      @assertEqual(1, s%count(ONE))
      @assertEqual(0, s%count(TWO))

      call s%insert(TWO)
      @assertEqual(1, s%count(ONE))
      @assertEqual(1, s%count(TWO))

      ! duplicate
      call s%insert(TWO)
      @assertEqual(1, s%count(ONE))
      @assertEqual(1, s%count(TWO))

   end subroutine testCount

#ifdef _pointer
! This test verifies that if two pointers are put in a Set
! they are treated as separate entries even if their targets
! have the same value.  (But the targets are not the same object.)

! This test is only relevant for types with _pointer.
@test(ifdef=_pointer)
   subroutine test_findSameTarget()
      type (Set) :: s

#  ifdef __type_allocatable_target
#    define __type_test_attrs , allocatable
#  else
#   define __type_test_attrs
#  endif

#define _ONE_  _ONE
      __type_declare_local :: pA
      __type_declare_local :: pB
      __type_declare_local :: pC
      __type_declare_target __type_test_attrs :: targA
      __type_declare_target __type_test_attrs :: targB
      __type_declare_target __type_test_attrs :: targC

      __type_declare_result, pointer :: qA, qB, qC

      type (SetIterator) :: iterA, iterB, iterC
      logical :: isNew

#ifdef _DEBUG
      type LocalWrapper
         integer, pointer :: item
      end type LocalWrapper
      type (LocalWrapper):: w
#endif

      __TYPE_INIT(pA, _ONE, targA)
      __TYPE_INIT(pB, _ONE, targB)
      __TYPE_INIT(pC, _ONE, targC)
      @assertFalse(associated(pA, pB))

#ifdef _DEBUG
      w%item => pA
      w%item => targB
      w%item => targC
#endif

      call s%insert(pA)
      @assertEqual(1, s%size())

      call s%insert(pB, isNew=isNew)
      @assertTrue(isNew)
      @assertEqual(2, s%size())

      call s%insert(pC, isNew=isNew)
      @assertTrue(isNew)
      @assertEqual(3, s%size())

      iterA = s%find(pA)
      iterB = s%find(pB)
      iterC = s%find(pC)

      qA => iterA%value()
      qB => iterB%value()
      qC => iterC%value()
      @assertFalse(associated(qA, qB))
      @assertFalse(associated(qA, qC))
      @assertFalse(associated(qB, qC))

   end subroutine test_findSameTarget
#endif

@test
   subroutine test_eraseOne()
      type (Set), target :: s
      type (SetIterator) :: iter

      call s%insert(ONE)
      call s%insert(THREE)
      call s%insert(FIVE)

      iter = s%find(THREE)
      call s%erase(iter)

      @assertEqual(2, s%size())
      @assertEqual(0, s%count(THREE))

      @assertEqual(1, s%count(ONE))
      @assertEqual(1, s%count(FIVE))

   end subroutine test_eraseOne

! In the case of containers of pointers, it is very difficult to know what
! is included in a range.  Thus we copy the set and use it as a reference.
@test
   subroutine test_eraseRange()
      type (Set), target :: s
      type (Set), target :: sCopy
      type (SetIterator) :: first
      type (SetIterator) :: last
      type (SetIterator) :: iter

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

      ! should delete THREE and FOUR (2 items)
      call s%erase(first, last)
      @assertTrue(first == last)

      @assertEqual(2, s%size())

      iter = sCopy%begin()
      call iter%next()
      last = sCopy%end()
      call last%prev()

      do while (iter /= last)
         q => iter%value()
         @assertEqual(0, s%count(q))
         call iter%next()
      end do

   end subroutine test_eraseRange
   
@test
   subroutine test_eraseAll()
      type (Set), target :: s
      type (SetIterator) :: first
      type (SetIterator) :: last

      call s%insert(ONE)
      call s%insert(THREE)
      call s%insert(FIVE)

      first = s%begin()
      last = s%end()

      call s%erase(first, last)

      @assertEqual(0, s%size())

      ! Iterator should now point to end of updated set.
      @assertTrue(first == s%end())

   end subroutine test_eraseAll


@test
   subroutine test_equalEmpty()
      type (Set) :: a, b

      @assertTrue(a == b)
      @assertFalse(a /= b)

   end subroutine test_equalEmpty


@test
   subroutine test_equal()
      type (Set), target :: a, b

      call a%insert(ONE)
      call a%insert(TWO)

      call b%insert(ONE)
      call b%insert(TWO)

      @assertTrue(a == b)
      @assertFalse(a /= b)

   end subroutine test_equal


@test
   subroutine test_notEqual()
      type (Set) :: a, b

      call a%insert(ONE)
      call a%insert(TWO)
      call a%insert(FOUR)

      call b%insert(ONE)
      call b%insert(TWO)
      call b%insert(FIVE)

      @assertFalse(a == b)
      @assertTrue(a /= b)

   end subroutine test_notEqual


   subroutine test_deepCopy()
      type (Set) :: a, b

      call a%insert(ONE)
      call a%insert(TWO)

      b = a
      @assertTrue(a == b)

      ! Shallow copy will show problems if we now insert an element
      ! and compare again.
      call b%insert(THREE)
      @assertTrue(a /= b)

   end subroutine test_deepCopy

   ! Ensure that deep copy obliterates any state the variable on the
   ! LHS had prior to the assignment.
@test
   subroutine test_deepCopy2()
      type (Set) :: a, b

      call a%insert(ONE)
      call a%insert(TWO)
      
      call b%insert(THREE)

      b = a
      @assertTrue(a == b)

      ! Shallow copy will show problems if we now insert an element
      ! and compare again.
      call b%insert(THREE)
      @assertTrue(a /= b)

   end subroutine test_deepCopy2
#include "templates/type_use_tokens_undef.inc"
      
end module Test_`'param()Set_mod
#include "templates/tmpltail.inc"

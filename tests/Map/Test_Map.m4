changecom()
#line 2 "tests/Map/Test_Map.m4"

module Test_`'KEY()`'VALUE()`'ALT()Map_mod
#include "types/key_`'KEY().inc"
#include "types/value_`'VALUE().inc"
#include "type_test_values/key_`'KEY().inc"
#include "type_test_values/value_`'VALUE().inc"
   use pFUnit_mod, only: assertTrue, assertFalse
   use pFUnit_mod, only: TestSuite, newTestSuite
   use pFUnit_mod, only: newTestMethod
   use pFUnit_mod, only: SourceLocation
   use pFUnit_mod, only: anyExceptions
   use pFUnit_mod, only: assertEqual
   use KEY()`'VALUE()`'ALT()Map_mod

#include "templates/tmplbase.inc"

#include "templates/key_set_use_tokens.inc"
#include "templates/key_template_macros.inc"
#include "templates/key_testing_macros.inc"

#include "templates/value_set_use_tokens.inc"
#include "templates/value_template_macros.inc"
#include "templates/value_testing_macros.inc"

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
   subroutine test_empty()
      type (Map) :: m

      @assertTrue(m%empty())
      call m%insert(KEY1, ONE)
      @assertFalse(m%empty())

   end subroutine test_empty

@test
   subroutine test_size()
      type (Map) :: m

      @assertEqual(0, m%size())
      call m%insert(KEY1, ONE)
      @assertEqual(1, m%size())
      call m%insert(KEY2, ONE)
      @assertEqual(2, m%size())

   end subroutine test_size


@test
   subroutine test_count()
      type (Map) :: m

      call m%insert(KEY1, ONE)
      call m%insert(KEY2, ONE)

      @assertEqual(1, m%count(KEY1))
      @assertEqual(1, m%count(KEY2))
      @assertEqual(0, m%count(KEY3))

   end subroutine test_count


@test
   subroutine test_max_size()
      type (Map) :: m

      @assertEqual(huge(1_SIZE_KIND), m%max_size())

   end subroutine test_max_size


@test
   subroutine test_at()
      type (Map) :: m
      __value_declare_result, pointer :: val
      
      call m%insert(KEY1, ONE)
      call m%insert(KEY2, TWO)

      val => m%at(KEY1)
      @assertEqual(ONE, val)

      val => m%at(KEY2)
      @assertEqual(TWO, val)

   end subroutine test_at

@test
   subroutine test_find()
      type (Map), target :: m
      type (MapIterator) :: iter

      call m%insert(KEY2, TWO)

      iter = m%find(KEY2)
      @assertEqual(TWO, iter%value())

      iter = m%find(KEY1)
      @assertTrue(iter == m%end())

      iter = m%find(KEY3)
      @assertTrue(iter == m%end())

   end subroutine test_find


@test
   subroutine test_erase()
      type (Map) :: m
      type (MapIterator) :: iter

      call m%insert(KEY1, ONE)
      iter = m%begin()

      call m%erase(iter)

      @assertEqual(0, m%size())

   end subroutine test_erase

@test
   subroutine test_next()
      type (Map) :: m
      type (MapIterator) :: iter

      __value_declare_result, pointer :: q1, q2, q3

      call m%insert(KEY1, ONE)
      call m%insert(KEY2, TWO)
      call m%insert(KEY3, THREE)

      iter = m%begin()
      q1 => iter%value()
      call iter%next()
      q2 => iter%value()
      call iter%next()
      q3 => iter%value()

      @assertFalse(associated(q1,q2))
      @assertFalse(associated(q1,q3))
      @assertFalse(associated(q2,q3))

   end subroutine test_next


@test
   subroutine test_previous()
      type (Map) :: m
      type (MapIterator) :: iter

      __value_declare_result, pointer :: q1, q2, q3

      call m%insert(KEY1, ONE)
      call m%insert(KEY2, TWO)
      call m%insert(KEY3, THREE)

      iter = m%end()
      call iter%previous()
      q3 => iter%value()
      call iter%previous()
      q2 => iter%value()
      call iter%previous()
      q1 => iter%value()

      @assertFalse(associated(q1,q2))
      @assertFalse(associated(q1,q3))
      @assertFalse(associated(q2,q3))

   end subroutine test_previous



@test
   subroutine test_iterGetValue()
      type (Map) :: m
      type (MapIterator) :: iter

      __value_declare_result, pointer :: q1, q2, q3

      call m%insert(KEY1, ONE)
      call m%insert(KEY2, TWO)
      call m%insert(KEY3, THREE)

      iter = m%begin()
      q1 => iter%value()

      call iter%next()
      q2 => iter%value()
      call iter%next()
      q3 => iter%value()

      @assertFalse(associated(q1,q2))
      @assertFalse(associated(q1,q3))
      @assertFalse(associated(q2,q3))

   end subroutine test_iterGetValue


@test
   subroutine testIsSet()
      type (Map) :: m
      logical :: f
      __value_declare_result, pointer :: val

      call m%set(KEY1,ONE)
      f = m%get(KEY1, val)
      @assertTrue(f)

   end subroutine testIsSet


@test
   subroutine testNotSet()
      type (Map) :: m
      logical :: f
      __value_declare_result, pointer :: val

      call m%set(KEY1,ONE)
      f = m%get(KEY2, val)
      @assertFalse(f)

   end subroutine testNotSet


@test
   subroutine testGet()
      type (Map) :: m
      logical :: f
      __value_declare_result, pointer :: val

      call m%set(KEY1,ONE)
      call m%set(KEY2,TWO)

      f = m%get(KEY1, val)
      @assertTrue(f)
      @assertEqual(ONE, val)

      f = m%get(KEY2, val)
      @assertTrue(f)
      @assertEqual(TWO, val)

   end subroutine testGet


   ! The following test crashes under gfortran 4.9 and 5.0.
   ! The theory is that a temp copy is incorrectly interacting
   ! with the FINAL method for SET.
@test(ifdef=include_broken)
   subroutine deepCopy()
      type (Map) :: m1, m2

      call m1%insert(KEY1, ONE)
      m2 = m1

   end subroutine deepCopy

#ifdef _alt
@test(ifdef=_alt)
   subroutine test_make_from_array_of_pairs()
      type (Map) :: m
      __value_declare_result, pointer :: val

      m =  Map([mapPair(KEY1,ONE), mapPair(KEY2,TWO), mapPair(KEY3,THREE)])
      @assertEqual(3, m%size())
      
      call m%insert(KEY1, ONE)
      call m%insert(KEY2, TWO)

      val => m%at(KEY1)
      @assertEqual(ONE, val)

      val => m%at(KEY2)
      @assertEqual(TWO, val)

   end subroutine test_make_from_array_of_pairs
#endif

#include "templates/type_use_tokens_undef.inc"
end module Test_`'KEY()`'VALUE()`'ALT()Map_mod
#include "templates/tmpltail.inc"



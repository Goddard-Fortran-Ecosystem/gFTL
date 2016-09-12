include(header.m4)

module Test_`'param()Vector_mod
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

      
   subroutine testSizeEmpty()
      type (Vector) :: v

       v = Vector()
       @assertEqual(0, v%size())

   end subroutine testSizeEmpty


   subroutine testEmpty()
      type (Vector) :: v

      v = Vector()
      @assertTrue(v%empty())

   end subroutine testEmpty

#ifndef __type_wrapped
@test(ifndef=__type_wrapped)
   subroutine testCopyFromArray_notEmpty()
      type (Vector) :: v

      v = [ONE]
      @assertFalse(v%empty())

   end subroutine testCopyFromArray_notEmpty

@test(ifndef=__type_wrapped)
   subroutine testCopyFromArray_size()
      type (Vector) :: v

      v = [ONE,TWO]
      @assertEqual(2, v%size())

   end subroutine testCopyFromArray_size
#endif

@test
   subroutine test_push_back_size()
      type (Vector) :: v

      v = Vector()
      call v%push_back(ONE)
      @assertEqual(1, v%size())

      call v%push_back(TWO)
      @assertEqual(2, v%size())

      call v%push_back(THREE)
      @assertEqual(3, v%size())

      call v%push_back(FOUR)
      @assertEqual(4, v%size())

      call v%push_back(FIVE)
      @assertEqual(5, v%size())

   end subroutine test_push_back_size


   ! front() should always return the 1st element, no matter how many
   ! pushes.
@test
   subroutine test_push_back_front()
      type (Vector) :: v
      __type_declare_result, pointer :: q

      v = Vector()
      call v%push_back(ONE)

      q => v%front()
      @assertEqual(ONE, q)

      call v%push_back(TWO)
      @assertEqual(2, v%size())

      q => v%front()
      @assertEqual(ONE, q)

      call v%push_back(THREE)
      @assertEqual(3, v%size())

      q => v%front()
      @assertEqual(ONE, q)

   end subroutine test_push_back_front


   subroutine test_push_back_back()
      type (Vector) :: v
      __type_declare_result, pointer :: q

      v = Vector()

      call v%push_back(ONE)
      q => v%back()
      @assertEqual(ONE, q)

      call v%push_back(TWO)
      q => v%back()
      @assertEqual(TWO, q)

      call v%push_back(THREE)
      q => v%back()
      @assertEqual(THREE, q)

      call v%push_back(FOUR)
      q => v%back()
      @assertEqual(FOUR, q)

      call v%push_back(FIVE)
      q => v%back()
      @assertEqual(FIVE, q)

   end subroutine test_push_back_back


! If the vector shrinks, there might be issues with
! elements that already have values.
@test
   subroutine test_push_back_shrink()
      type (Vector) :: v
      __type_declare_result, pointer :: q

      v = Vector()

      call v%push_back(ONE)
      q => v%back()
      @assertEqual(ONE, q)

      call v%push_back(TWO)
      q => v%back()
      @assertEqual(TWO, q)

      call v%push_back(THREE)
      q => v%back()
      @assertEqual(THREE, q)

      call v%resize(2)
      call v%push_back(FOUR)
      q => v%back()
      @assertEqual(FOUR, q)

      call v%resize(1)
      call v%push_back(FIVE)
      q => v%back()
      @assertEqual(FIVE, q)

   end subroutine test_push_back_shrink


@test
   subroutine test_at()
      type (Vector) :: v
      __type_declare_result, pointer :: q

      v = Vector()

      call v%push_back(ONE)
      call v%push_back(TWO)
      call v%push_back(THREE)
      call v%push_back(FOUR)
      call v%push_back(FIVE)

      q => v%at(1)
      @assertEqual(ONE, q)
      q => v%at(2)
      @assertEqual(TWO, q)
      q => v%at(3)
      @assertEqual(THREE, q)
      q => v%at(4)
      @assertEqual(FOUR, q)
      q => v%at(5)
      @assertEqual(FIVE, q)

   end subroutine test_at

@test
   subroutine test_of()
      type (Vector) :: v
      __type_declare_result, pointer :: q

      v = Vector()

      call v%push_back(ONE)
      call v%push_back(TWO)
      call v%push_back(THREE)
      call v%push_back(FOUR)
      call v%push_back(FIVE)

      q => v%of(1)
      @assertEqual(ONE, q)
      q => v%of(2)
      @assertEqual(TWO, q)
      q => v%of(3)
      @assertEqual(THREE, q)
      q => v%of(4)
      @assertEqual(FOUR, q)
      q => v%of(5)
      @assertEqual(FIVE, q)

    end subroutine test_of

@test
    subroutine test_get()
      type (Vector) :: v
      __type_declare_component :: q

      v = Vector()

      call v%push_back(ONE)
      call v%push_back(TWO)
      call v%push_back(THREE)
      call v%push_back(FOUR)
      call v%push_back(FIVE)

      __TYPE_ASSIGN(q, v%get(1))
      @assertEqual(ONE, q)
      __TYPE_FREE(q)
      __TYPE_ASSIGN(q, v%get(2))
      @assertEqual(TWO, q)
      __TYPE_FREE(q)
      __TYPE_ASSIGN(q, v%get(3))
      @assertEqual(THREE, q)
      __TYPE_FREE(q)
      __TYPE_ASSIGN(q, v%get(4))
      @assertEqual(FOUR, q)
      __TYPE_FREE(q)
      __TYPE_ASSIGN(q, v%get(5))
      @assertEqual(FIVE, q)

   end subroutine test_get


@test
   subroutine test_get_negativeIndex()
      type (Vector) :: v
      __type_declare_component :: q


      call v%push_back(ONE)
      call v%push_back(TWO)
      call v%push_back(THREE)
      call v%push_back(FOUR)
      call v%push_back(FIVE)

      __TYPE_ASSIGN(q, v%get(0))
      @assertEqual(FIVE, q)
      __TYPE_FREE(q)
      __TYPE_ASSIGN(q, v%get(-1))
      @assertEqual(FOUR, q)
      __TYPE_FREE(q)
      __TYPE_ASSIGN(q, v%get(-2))
      @assertEqual(THREE, q)
      __TYPE_FREE(q)
      __TYPE_ASSIGN(q, v%get(-3))
      @assertEqual(TWO, q)
      __TYPE_FREE(q)
      __TYPE_ASSIGN(q, v%get(-4))
      @assertEqual(ONE, q)

   end subroutine test_get_negativeIndex

   ! Verify that non-poiter assignment of the return from at() does
   ! not modify the internal structure of the vector.
   ! This is really a test of the compiler not the implementation.
   ! The use case does not make sense for polymorphic targets, as assignment
   ! must reallocate the target and thus invalidate the pointer
@test   
   subroutine test_atModify()
      type (Vector) :: v
      __type_declare_result, pointer :: q1, q2, qt

      v = Vector()
      call v%push_back(ONE)
      call v%push_back(TWO)
      call v%push_back(FOUR)

      q1 => v%at(1)
      qt => v%at(1) ! workaround for ifort 15 with Unlimited Polymorphic Entity
#ifdef _allocatable
      ! Cannot do direct assignment of polymorphic targets.
      ! Allocation is always implied, even with F2008 allocate on
      ! assignment for polymorphic entities.  This use case is a bit
      ! silly, but it does not hurt to include it.
      allocate(qt, source=ONE_B)
#else
      qt = ONE_B
#endif
      q2 => v%at(1)
      @assertTrue(associated(q1,q2))

   end subroutine test_atModify


   ! Ensure that resizing uses the default value if
   ! provided.   
@test
   subroutine test_resizeGrow()
      type (Vector) :: v
      __type_declare_result, pointer :: q

      v = Vector()
      call v%push_back(ONE)
      call v%push_back(TWO)
      call v%push_back(FOUR)

      call v%resize(10, FIVE)

      @assertEqual(10, v%size())
      q => v%at(4)
      @assertEqual(FIVE, q)
      q => v%at(5)
      @assertEqual(FIVE, q)
      q => v%at(10)
      @assertEqual(FIVE, q)

   end subroutine test_resizeGrow


@test
   subroutine test_resizeShrink()
      type (Vector) :: v
      __type_declare_result, pointer :: q

      v = Vector()
      call v%push_back(ONE)
      call v%push_back(TWO)
      call v%push_back(THREE)
      call v%push_back(FOUR)

      call v%resize(2)

      @assertEqual(2, v%size())
      q => v%at(1)
      @assertEqual(ONE, q)
      q => v%at(2)
      @assertEqual(TWO, q)

   end subroutine test_resizeShrink

   
@test
   subroutine test_reserve_capacity()
      type (Vector) :: v

      v = Vector()
      @assertTrue(0 <= v%capacity())

      call v%push_back(ONE)
      @assertTrue(1 <= v%capacity())

      call v%reserve(8)
      @assertTrue(8 <= v%capacity())

   end subroutine test_reserve_capacity

   
   subroutine test_shrink_to_fit()
      type (Vector) :: v
      v = Vector()
      call v%shrink_to_fit()
      @assertTrue(0 <= v%capacity())
      call v%reserve(7)

      call v%push_back(ONE)
      call v%shrink_to_fit()
      @assertTrue(1 <= v%capacity())

      call v%push_back(TWO)
      call v%shrink_to_fit()
      @assertTrue(2 <= v%capacity())

      ! check to make certain vector still has correct elements
      @assertEqual(TWO, v%at(2))

   end subroutine test_shrink_to_fit
   

@test
   subroutine test_pop_back()
      type (Vector) :: v

      v = Vector()
      call v%push_back(ONE)
      call v%push_back(TWO)
      call v%push_back(THREE)

      call v%pop_back()
      
      @assertEqual(2, v%size())
      
      @assertEqual(ONE, v%at(1))
      @assertEqual(TWO, v%at(2))

   end subroutine test_pop_back

   ! This test checks that an insertion to an empty container 
   ! acts like push_back.
@test
   subroutine test_insertEmpty()
      type (Vector) :: v

      v = Vector()
      call v%insert(1,value=FIVE)
      
      @assertEqual(1, v%size())
      @assertEqual(FIVE, v%at(1))

   end subroutine test_insertEmpty


   ! This test checks that an insertion at the beginning of the vector
   ! correctly adjusts the location of subsequest elements.
@test
   subroutine test_insertBeginning()
      type (Vector) :: v

      v = Vector()
      call v%push_back(ONE)
      call v%push_back(TWO)
      call v%push_back(THREE)

      call v%insert(1,value=FIVE)
      
      @assertEqual(4, v%size())
      @assertEqual(FIVE, v%at(1))

      @assertEqual(ONE, v%at(2))
      @assertEqual(TWO, v%at(3))
      @assertEqual(THREE, v%at(4))

   end subroutine test_insertBeginning


   ! This test checks that an insertion into the middle of the vector
   ! correctly adjusts the location of subsequest elements.
@test
   subroutine test_insertMiddle()
      type (Vector) :: v

      v = Vector()
      call v%push_back(ONE)
      call v%push_back(TWO)
      call v%push_back(THREE)

      call v%insert(2,value=FIVE)
      
      @assertEqual(4, v%size())
      @assertEqual(FIVE, v%at(2))

      @assertEqual(ONE, v%at(1))
      @assertEqual(TWO, v%at(3))
      @assertEqual(THREE, v%at(4))

   end subroutine test_insertMiddle


   ! This test checks that an insertion at the end of the vector
   ! leaves other elements in place.
@test
   subroutine test_insertEnd()
      type (Vector) :: v

      v = Vector()
      call v%push_back(ONE)
      call v%push_back(TWO)
      call v%push_back(THREE)

      call v%insert(4,value=FIVE)
      
      @assertEqual(4, v%size())
      @assertEqual(FIVE, v%at(4))

      @assertEqual(ONE, v%at(1))
      @assertEqual(TWO, v%at(2))
      @assertEqual(THREE, v%at(3))

   end subroutine test_insertEnd

@test
   subroutine test_swap()
      type (Vector) :: v1, v2

      v1 = Vector()
      call v1%push_back(ONE)
      call v1%push_back(TWO)
      call v1%push_back(THREE)

      v2 = Vector()
      call v2%push_back(FOUR)
      call v2%push_back(FIVE)

      call swap(v1, v2)

      @assertEqual(2, v1%size())
      @assertEqual(3, v2%size())

      @assertEqual(FOUR, v1%at(1))
      @assertEqual(FIVE, v1%at(2))

      @assertEqual(ONE, v2%at(1))
      @assertEqual(TWO, v2%at(2))
      @assertEqual(THREE, v2%at(3))

   end subroutine test_swap

@test
   subroutine test_swap_method()
      type (Vector) :: v1, v2

      v1 = Vector()
      call v1%push_back(ONE)
      call v1%push_back(TWO)
      call v1%push_back(THREE)

      v2 = Vector()
      call v2%push_back(FOUR)
      call v2%push_back(FIVE)

      call v1%swap(v2)

      @assertEqual(2, v1%size())
      @assertEqual(3, v2%size())

      @assertEqual(FOUR, v1%at(1))
      @assertEqual(FIVE, v1%at(2))

      @assertEqual(ONE, v2%at(1))
      @assertEqual(TWO, v2%at(2))
      @assertEqual(THREE, v2%at(3))

   end subroutine test_swap_method

@test
   subroutine test_copy()
      type (Vector) :: v1, v2
      __type_declare_result, pointer :: q

      v1 = Vector()
      call v1%push_back(ONE)
      call v1%push_back(THREE)
      call v1%push_back(FIVE)

      v2 = Vector()
      call v2%push_back(ONE)


      v1 = v2
      @assertEqual(1, v1%size())
      q => v1%at(1)
      @assertEqual(ONE, q)

   end subroutine test_copy


@test
   subroutine test_clear()
      type (Vector) :: v

      v = Vector()
      call v%push_back(ONE)
      call v%push_back(THREE)
      call v%push_back(FIVE)

      call v%clear()
      @assertEqual(0, v%size())

   end subroutine test_clear


@test
   subroutine test_eraseOne()
      type (Vector) :: v
      type (VectorIterator) :: iter

      v = Vector()
      call v%push_back(ONE)
      call v%push_back(THREE)
      call v%push_back(FIVE)

      iter = v%begin()
      call iter%next()

      call v%erase(iter)
      @assertEqual(2, v%size())

      @assertEqual(ONE, v%at(1))
      @assertEqual(FIVE, v%at(2))

   end subroutine test_eraseOne


@test
   subroutine test_eraseRange()
      type (Vector) :: v
      type (VectorIterator) :: first
      type (VectorIterator) :: last

      v = Vector()
      call v%push_back(ONE)
      call v%push_back(THREE)
      call v%push_back(FIVE)

      first = v%begin()
      last = v%begin()
      call last%next()
      call last%next()

      call v%erase(first, last)

      @assertEqual(1, v%size())
      @assertEqual(FIVE, v%at(1))

   end subroutine test_eraseRange


@test   
   subroutine test_eraseToEnd()
      type (Vector) :: v
      type (VectorIterator) :: iter
      type (VectorIterator) :: last

      v = Vector()
      call v%push_back(ONE)
      call v%push_back(THREE)
      call v%push_back(FIVE)

      iter = v%begin()
      call iter%next()
      last = v%end()

      call v%erase(iter, last)

      @assertEqual(1, v%size())
      @assertEqual(ONE, v%at(1))

      ! Iterator should now point to end of updated vector.
      @assertTrue(iter == v%end())

   end subroutine test_eraseToEnd
   

@test
   subroutine test_eraseOneCheckIter()
      type (Vector) :: v
      type (VectorIterator) :: iter

      v = Vector()
      call v%push_back(ONE)
      call v%push_back(TWO)
      call v%push_back(THREE)

      iter = v%begin()
      call iter%next()

      call v%erase(iter)
      @assertEqual(THREE, iter%get())
      
   end subroutine test_eraseOneCheckIter


@test
   subroutine test_erase_lastElement()
      type (Vector) :: v
      type (VectorIterator) :: iter

      v = Vector()
      call v%push_back(ONE)
      call v%push_back(TWO)
      call v%push_back(THREE)

      iter = v%begin()
      call iter%next()
      call iter%next()

      call v%erase(iter)
      @assertEqual(2, v%size())
      @assertEqual(ONE, v%at(1))
      @assertEqual(TWO, v%at(2))

   end subroutine test_erase_lastElement

! This test is motivated by a runtime error experinced in gfortran
! with allocatable items.  Deallocation was not happening for things
! off the tail of the vector.
@test
   subroutine test_reuse_after_erase()
      type (Vector) :: v
      type (VectorIterator) :: first

      v = Vector()
      call v%push_back(ONE)
      first = v%begin()
      call v%erase(first)

      call v%push_back(ONE)

      @assertEqual(1, v%size())
      @assertEqual(ONE, v%at(1))

   end subroutine test_reuse_after_erase


#ifdef _equal_defined
@test(ifdef=_equal_defined)
   subroutine test_get_index()
      type (Vector) :: v

      v = Vector()
      call v%push_back(ONE)

      @assertEqual(1, v%get_index(ONE))
      @assertEqual(0, v%get_index(TWO))

      call v%push_back(TWO)
      @assertEqual(1, v%get_index(ONE))
      @assertEqual(2, v%get_index(TWO))

   end subroutine test_get_index
#endif


#ifdef _equal_defined
@test(ifdef=_equal_defined)
   subroutine test_equal_empty()
      type (Vector) :: v

      v = Vector()
      @assertTrue(v == v)
      @assertFalse(v /= v)

   end subroutine test_equal_empty
#endif


#ifdef _equal_defined
@test(ifdef=_equal_defined)
   subroutine test_equal_equal()
      type (Vector) :: v

      v = Vector()
      call v%push_back(ONE)
      call v%push_back(TWO)

      @assertTrue(v == v)
      @assertFalse(v /= v)

   end subroutine test_equal_equal
#endif

#ifdef _equal_defined
@test(ifdef=_equal_defined)
   subroutine test_equal_unequal_size()
      type (Vector) :: v1, v2

      v1 = Vector()
      call v1%push_back(ONE)
      call v1%push_back(TWO)

      v2 = v1
      call v2%push_back(THREE)
      
      @assertFalse(v1 == v2)
      @assertTrue(v1 /= v2)

   end subroutine test_equal_unequal_size
#endif

#ifdef _equal_defined
@test(ifdef=_equal_defined)
   subroutine test_equal_unequal_element()
      type (Vector) :: v1, v2

      v1 = Vector()
      call v1%push_back(ONE)
      v2 = v1

      call v1%push_back(TWO)
      call v2%push_back(THREE)
      
      @assertFalse(v1 == v2)
      @assertTrue(v1 /= v2)

   end subroutine test_equal_unequal_element
#endif

#ifdef __type_compare_well_defined
@test(ifdef=__type_compare_well_defined)
   subroutine test_less_than_empty()
      type (Vector) :: v

      v = Vector()
      @assertFalse(v < v)
      @assertTrue(v >= v)

   end subroutine test_less_than_empty


@test(ifdef=__type_compare_well_defined)
   subroutine test_less_than_same()
      type (Vector) :: v

      v = Vector()
      call v%push_back(ONE)

      @assertFalse(v < v)
      @assertTrue(v >= v)
   end subroutine test_less_than_same


@test(ifdef=__type_compare_well_defined)
   subroutine test_less_than_different()
      type (Vector) :: v1, v2

      v1 = Vector()
      call v1%push_back(ONE)

      v2 = v1
      call v2%push_back(TWO)  ! TWO > ONE

      @assertTrue(v1 < v2)
      @assertFalse(v1 >= v2)

      call v1%push_back(TWO)
      call v1%push_back(THREE)  ! THREE >= ONE
      
      @assertFalse(v1 < v2)
      @assertTrue(v1 >= v2)

   end subroutine test_less_than_different


@test(ifdef=__type_compare_well_defined)
   subroutine test_greater_than_empty()
      type (Vector) :: v

      v = Vector()
      @assertFalse(v > v)
      @assertTrue(v <= v)
      
   end subroutine test_greater_than_empty


@test(ifdef=__type_compare_well_defined)
   subroutine test_greater_than_same()
      type (Vector) :: v

      v = Vector()
      call v%push_back(ONE)

      @assertFalse(v > v)
      @assertTrue(v <= v)
      
   end subroutine test_greater_than_same


@test(ifdef=__type_compare_well_defined)
   subroutine test_greater_than_different()
      type (Vector) :: v1, v2

      v1 = Vector()
      call v1%push_back(ONE)

      v2 = v1
      call v2%push_back(TWO)  ! TWO > ONE

      @assertFalse(v1 > v2)
      @assertTrue(v1 <= v2)

      call v1%push_back(TWO)
      call v1%push_back(THREE)  ! THREE >= ONE
      
      @assertTrue(v1 > v2)
      @assertFalse(v1 <= v2)
   end subroutine test_greater_than_different
#endif


@test
   subroutine test_set()
      type (Vector) :: v

      call v%push_back(ONE)
      call v%push_back(TWO)
      
      call v%set(1, THREE)

      @assertEqual(2, v%size())
      @assertEqual(THREE, v%get(1))
      @assertEqual(TWO, v%get(2))

#ifdef _pointer
     block
         __type_declare_result, pointer :: q
! Check that pointer is changed, not just target.
        q => v%at(1)
        @assertFalse(associated(ONE, q))
        @assertTrue(associated(THREE, q))
     end block
#endif

   end subroutine test_set


@test
   subroutine test_set_back()
      type (Vector) :: v

      call v%push_back(ONE)
      call v%push_back(TWO)
      
      call v%set(0, THREE)
      @assertEqual(2, v%size())
      @assertEqual(ONE, v%get(1))
      @assertEqual(THREE, v%get(2))
   end subroutine test_set_back


#include "templates/type_use_tokens_undef.inc"

end module Test_`'param()Vector_mod
#include "templates/tmpltail.inc"



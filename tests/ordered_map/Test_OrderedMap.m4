changecom()
changequote(`{',`}')
module Test_{}_key(){}_type()OrderedMap
   use, intrinsic :: iso_fortran_env
   use funit
   use _key(){}_type()OrderedMap_mod
   ifelse(_type(),{Foo},{use Foo_mod},_key(),{Foo},{use Foo_mod})

#include "_key()_Key.inc"
#include "_type()_T.inc"
#include "shared/define_common_macros.inc"
#include "test_{}_key()_Key.inc"
#include "test_{}_type()_T.inc"

#include "parameters/Key/copy_Key_to_map_Key.inc"
#include "parameters/Key/copy_map_Key_to_internal_Key.inc"
#include "parameters/Key/define_derived_macros.inc"

#include "parameters/T/copy_T_to_map_T.inc"
#include "parameters/T/copy_map_T_to_internal_T.inc"
#include "parameters/T/define_derived_macros.inc"

   __Key_declare_component__ :: key_zero
   __Key_declare_component__ :: key_one
   __Key_declare_component__ :: key_two
   __Key_declare_component__ :: key_three

   __T_declare_component__ :: zero
   __T_declare_component__ :: one
   __T_declare_component__ :: two
   __T_declare_component__ :: three

contains


@before
   subroutine setUp()

      key_zero = _key_zero
      key_one= _key_one
      key_two = _key_two
      key_three = _key_three

      zero = _zero
      one = _one
      two = _two
      three = _three

   end subroutine setUp



@test
   subroutine test_empty()
      type (OrderedMap) :: m

      @assertTrue(m%empty())
      call m%insert(key_one, one)
      @assertFalse(m%empty())

   end subroutine test_empty

@test
   subroutine test_size()
      type (OrderedMap) :: m

      @assert_that(int(m%size()), is(equal_to(0)))
      call m%insert(key_one, one)
      @assert_that(int(m%size()), is(equal_to(1)))
      call m%insert(key_two, one)
      @assert_that(int(m%size()), is(equal_to(2)))

   end subroutine test_size


@test
   subroutine test_count()
      type (OrderedMap) :: m

      call m%insert(key_one, one)
      call m%insert(key_two, one)

      @assert_that(m%count(key_one), is(equal_to(1_GFTL_SIZE_KIND)))
      @assert_that(m%count(key_two), is(equal_to(1_GFTL_SIZE_KIND)))
      @assert_that(m%count(key_three), is(equal_to(0_GFTL_SIZE_KIND)))

   end subroutine test_count


@test
   subroutine test_max_size()
      type (OrderedMap) :: m

      @assert_that(m%max_size(), is(equal_to(huge(1_GFTL_SIZE_KIND))))

   end subroutine test_max_size


@test
   subroutine test_at()
      type (OrderedMap) :: m
      __T_declare_result__, pointer :: val
      
      call m%insert(key_one, one)
      call m%insert(key_two, two)

      val => m%at(key_one)
      @assert_that(val, is(equal_to(one)))

      val => m%at(key_two)
      @assert_that(val, is(equal_to(two)))

   end subroutine test_at

@test
   subroutine test_value_empty_is_null()
      type (OrderedMap), target :: m
      type (OrderedMapIterator) :: iter

      iter = m%find(key_one)
      @assertFalse(associated(iter%second()))

   end subroutine test_value_empty_is_null

@test
   subroutine test_find()
      type (OrderedMap), target :: m
      type (OrderedMapIterator) :: iter

      call m%insert(key_two, two)

      iter = m%find(key_two)
      @assert_that(iter%second(), is(equal_to(two)))

      iter = m%find(key_one)
      @assertTrue(iter == m%end())

      iter = m%find(key_three)
      @assertTrue(iter == m%end())

   end subroutine test_find


@test
   subroutine test_erase()
      type (OrderedMap), target :: m
      type (OrderedMapIterator) :: iter

      call m%insert(key_one, one)
      iter = m%begin()

      iter = m%erase(iter)

      @assert_that(int(m%size()), is(equal_to(0)))

   end subroutine test_erase

@test
   subroutine test_next()
      type (OrderedMap), target :: m
      type (OrderedMapIterator) :: iter

      __T_declare_result__, pointer :: q1, q2, q3

      call m%insert(key_one, one)
      call m%insert(key_two, two)
      call m%insert(key_three, THREE)

      iter = m%begin()
      q1 => iter%second()
      call iter%next()
      q2 => iter%second()
      call iter%next()
      q3 => iter%second()

      @assertFalse(associated(q1,q2))
      @assertFalse(associated(q1,q3))
      @assertFalse(associated(q2,q3))

   end subroutine test_next

@test
   subroutine test_order()
      type (OrderedMap), target :: m
      type (OrderedMapIterator) :: iter

      __T_declare_result__, pointer :: q1, q2, q3

      call m%insert(key_one, one)
      call m%insert(key_two, two)
      call m%insert(key_three, three)

      iter = m%begin()
      q1 => iter%second()
      @assert_that(q1, is(equal_to(one)))
      call iter%next()
      q2 => iter%second()
      @assert_that(q2, is(equal_to(two)))
      call iter%next()
      q3 => iter%second()
      @assert_that(q3, is(equal_to(three)))

      ! now reverse order to be safe
      call m%clear
      call m%insert(key_one, three)
      call m%insert(key_two, two)
      call m%insert(key_three, one)

      iter = m%begin()
      q1 => iter%second()
      @assert_that(q1, is(equal_to(three)))
      call iter%next()
      q2 => iter%second()
      @assert_that(q2, is(equal_to(two)))
      call iter%next()
      q3 => iter%second()
      @assert_that(q3, is(equal_to(one)))
   end subroutine test_order

@test
   subroutine test_prev()
      type (OrderedMap), target :: m
      type (OrderedMapIterator) :: iter

      __T_declare_result__, pointer :: q1, q2, q3

      call m%insert(key_one, one)
      call m%insert(key_two, two)
      call m%insert(key_three, three)

      iter = m%end()
      call iter%prev()
      q3 => iter%second()
      call iter%prev()
      q2 => iter%second()
      call iter%prev()
      q1 => iter%second()

      @assertFalse(associated(q1,q2))
      @assertFalse(associated(q1,q3))
      @assertFalse(associated(q2,q3))

   end subroutine test_prev



@test
   subroutine test_iterGetValue()
      type (OrderedMap), target :: m
      type (OrderedMapIterator) :: iter

      __T_declare_result__, pointer :: q1, q2, q3

      call m%insert(key_one, one)
      call m%insert(key_two, two)
      call m%insert(key_three, THREE)

      iter = m%begin()
      q1 => iter%second()

      call iter%next()
      q2 => iter%second()
      call iter%next()
      q3 => iter%second()

      @assertFalse(associated(q1,q2))
      @assertFalse(associated(q1,q3))
      @assertFalse(associated(q2,q3))

   end subroutine test_iterGetValue


@test
   subroutine testIsSet()
      type (OrderedMap) :: m
      logical :: f
      __T_declare_result__, pointer :: val

      call m%set(key_one,one)
      f = m%get(key_one, val)
      @assertTrue(f)

   end subroutine testIsSet


@test
   subroutine testNotSet()
      type (OrderedMap) :: m
      logical :: f
      __T_declare_result__, pointer :: val

      call m%set(key_one,one)
      f = m%get(key_two, val)
      @assertFalse(f)

   end subroutine testNotSet


@test
   subroutine testGet()
      type (OrderedMap), target :: m
      logical :: f
      __T_declare_result__, pointer :: val

      call m%set(key_one,one)
      call m%set(key_two,two)

      f = m%get(key_one, val)
      @assertTrue(f)
      @assert_that(val, is(equal_to(one)))

      f = m%get(key_two, val)
      @assertTrue(f)
      @assert_that(val, is(equal_to(two)))

   end subroutine testGet


   ! The following test crashes under gfortran 4.9 and 5.0.
   ! The theory is that a temp copy is incorrectly interacting
   ! with the FINAL method for SET.
@test(ifdef=include_broken)
   subroutine deepCopy()
      type (OrderedMap) :: m1, m2

      call m1%insert(key_one, one)
      m2 = m1

   end subroutine deepCopy

#ifdef _alt
@test(ifdef=_alt)
   subroutine test_make_from_array_of_pairs()
      type (OrderedMap) :: m
      __T_declare_result__, pointer :: val

      m =  Map([mapPair(key_one,one), mapPair(key_two,two), mapPair(key_three,THREE)])
      @assert_that(int(m%size()), is(equal_to(3)))
      
      call m%insert(key_one, one)
      call m%insert(key_two, two)

      val => m%at(key_one)
      @assert_that(val, is(equal_to(one)))

      val => m%at(key_two)
      @assert_that(val, is(equal_to(two)))

   end subroutine test_make_from_array_of_pairs
#endif

#include "parameters/Key/undef_derived_macros.inc"
#include "parameters/Key/undef_internal.inc"
#include "parameters/Key/undef_map_Key.inc"

#include "parameters/T/undef_derived_macros.inc"
#include "parameters/T/undef_internal.inc"
#include "parameters/T/undef_map_T.inc"

#include "shared/undef_common_macros.inc"
end module Test_{}_key(){}_type()OrderedMap



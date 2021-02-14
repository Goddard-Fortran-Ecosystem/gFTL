changecom()
changequote(`{',`}')
module Test_{}_type()DequeRIterator
   use funit
   use _type()Deque_mod
   ifelse(_type(),{Foo},{use Foo_mod})
   ifelse(_type(),{FooPoly},{use Foo_mod})

#include "_type().inc"
#include "shared/define_common_macros.inc"
#include "test_{}_type().inc"
#include "parameters/T/copy_T_to_deque_T.inc"
#include "parameters/T/copy_deque_T_to_internal_T.inc"
#include "parameters/T/define_derived_macros.inc"

   __T_declare_component__ :: zero
   __T_declare_component__ :: one
   __T_declare_component__ :: two
   __T_declare_component__ :: three

define({ASSERT},{
#if (__T_type_id__ == __CHARACTER__) && defined(__GFORTRAN__)
@assertEqual({$1},{$2})
#else
@assert_that({$1},is(equal_to({$2})))
#endif})

contains

   @before
   subroutine setup()

      zero = _zero
      one = _one
      two = _two
      three = _three

   end subroutine setup

   @test
   subroutine test_of()
      type(Deque), target :: v
      type(DequeRIterator) :: iter

      call v%push_back(one)
      call v%push_back(two)
      call v%push_back(three)

      iter = v%rbegin()
      ASSERT(iter%of(), three)

      iter = v%rend() - 1
      ASSERT(iter%of(), one)

   end subroutine test_of

   @test
   subroutine test_of_offset_default()
      type(Deque), target :: v
      type(DequeRIterator) :: iter

      call v%push_back(one)
      call v%push_back(two)
      call v%push_back(three)

      iter = v%rbegin() + 1
      ASSERT(iter%of(-1), three)
      ASSERT(iter%of(+0), two)
      ASSERT(iter%of(+1), one)

   end subroutine test_of_offset_default

   @test
   subroutine test_of_offset_size_kind()
      type(Deque), target :: v
      type(DequeRIterator) :: iter

      call v%push_back(one)
      call v%push_back(two)
      call v%push_back(three)

      iter = v%rbegin() + 1
      ASSERT(iter%of(-1_GFTL_SIZE_KIND), three)
      ASSERT(iter%of(+0_GFTL_SIZE_KIND), two)
      ASSERT(iter%of(+1_GFTL_SIZE_KIND), one)

   end subroutine test_of_offset_size_kind

   @test
   subroutine test_add()
      type(Deque), target :: v
      type(DequeRIterator) :: iter

      call v%push_back(one)
      call v%push_back(two)
      call v%push_back(three)

      iter = v%rbegin()
      call iter%add(2)
      ASSERT(iter%of(), one)
      
      call iter%sub(2)
      ASSERT(iter%of(), three)

      call iter%add(1_GFTL_SIZE_KIND)
      ASSERT(iter%of(), two)

      call iter%sub(1_GFTL_SIZE_KIND)
      ASSERT(iter%of(), three)

   end subroutine test_add

   @test
   subroutine test_add_operator()
      type(Deque), target :: v
      type(DequeRIterator) :: iter, new_iter

      call v%push_back(one)
      call v%push_back(two)
      call v%push_back(three)

      iter = v%rbegin()
      new_iter = iter + 2
      ASSERT(new_iter%of(), one)
      
      new_iter = new_iter - 2
      ASSERT(new_iter%of(), three)

      new_iter = iter + 1_GFTL_SIZE_KIND
      ASSERT(new_iter%of(), two)

      new_iter = new_iter - 1_GFTL_SIZE_KIND
      ASSERT(new_iter%of(), three)

   end subroutine test_add_operator

   @test
   subroutine test_equal()
      type(Deque), target :: v
      type(DequeRIterator) :: ia, ib

      call v%push_back(one)
      call v%push_back(two)
      call v%push_back(three)

      ia = v%rbegin()
      ib = ia + 1

      @assert_that(ia == ia, is(true()))
      @assert_that(ia == ib, is(false()))
      @assert_that(ib == ib, is(true()))
      
      @assert_that(ia /= ia, is(false()))
      @assert_that(ia /= ib, is(true()))
      @assert_that(ib /= ib, is(false()))
      
      
   end subroutine test_equal
   
   @test
   subroutine test_less_than()
      type(Deque), target :: v
      type(DequeRIterator) :: ia, ib

      call v%push_back(one)
      call v%push_back(two)
      call v%push_back(three)

      ia = v%rbegin()
      ib = ia + 1

      @assert_that(ia < ia, is(false()))
      @assert_that(ia < ib, is(true()))
      @assert_that(ib < ia, is(false()))
      @assert_that(ib < ib, is(false()))

   end subroutine test_less_than

   @test
   subroutine test_less_than_or_equal()
      type(Deque), target :: v
      type(DequeRIterator) :: ia, ib

      call v%push_back(one)
      call v%push_back(two)
      call v%push_back(three)

      ia = v%rbegin()
      ib = ia + 1

      @assert_that(ia <= ia, is(true()))
      @assert_that(ia <= ib, is(true()))
      @assert_that(ib <= ia, is(false()))
      @assert_that(ib <= ib, is(true()))

   end subroutine test_less_than_or_equal

   @test
   subroutine test_greater_than()
      type(Deque), target :: v
      type(DequeRIterator) :: ia, ib

      call v%push_back(one)
      call v%push_back(two)
      call v%push_back(three)

      ia = v%rbegin()
      ib = ia + 1

      @assert_that(ia > ia, is(false()))
      @assert_that(ia > ib, is(false()))
      @assert_that(ib > ia, is(true()))
      @assert_that(ib > ib, is(false()))

   end subroutine test_greater_than

   @test
   subroutine test_greater_than_or_equal()
      type(Deque), target :: v
      type(DequeRIterator) :: ia, ib

      call v%push_back(one)
      call v%push_back(two)
      call v%push_back(three)

      ia = v%rbegin()
      ib = ia + 1

      @assert_that(ia >= ia, is(true()))
      @assert_that(ia >= ib, is(false()))
      @assert_that(ib >= ia, is(true()))
      @assert_that(ib >= ib, is(true()))

   end subroutine test_greater_than_or_equal

   @test
   subroutine test_next_method()
      type(Deque), target :: v
      type(DequeRIterator) :: iter

      call v%push_back(one)
      call v%push_back(two)

      iter = v%rbegin()
      call iter%next()
      @assert_that(iter == v%rend(), is(false()))
      call iter%next()
      @assert_that(iter == v%rend(), is(true()))
      
   end subroutine test_next_method

   @test
   subroutine test_prev_method()
      type(Deque), target :: v
      type(DequeRIterator) :: iter

      call v%push_back(one)
      call v%push_back(two)

      iter = v%rend()
      call iter%prev()
      @assert_that(iter == v%rbegin(), is(false()))
      ASSERT(iter%of(), one)

      call iter%prev()
      @assert_that(iter == v%rbegin(), is(true()))
      
   end subroutine test_prev_method

   @test
   subroutine test_next()
      type(Deque), target :: v
      type(DequeRIterator) :: iter

      call v%push_back(one)
      call v%push_back(two)

      iter = next(v%rbegin(),1)
      ASSERT(iter%of(), one)

      iter = next(v%rbegin(),2)
      @assert_that(iter == v%rend(), is(true()))

      iter = next(v%rbegin(),1_GFTL_SIZE_KIND)
      ASSERT(iter%of(), one)

      iter = next(v%rbegin(),2_GFTL_SIZE_KIND)
      @assert_that(iter == v%rend(), is(true()))

      
   end subroutine test_next

   @test
   subroutine test_prev()
      type(Deque), target :: v
      type(DequeRIterator) :: iter

      call v%push_back(one)
      call v%push_back(two)
      call v%push_back(three)

      iter = prev(v%rend(),1)
      ASSERT(iter%of(), one)
      iter = prev(v%rend(),3)
      ASSERT(iter%of(), three)
      
      iter = prev(v%rend(),1_GFTL_SIZE_KIND)
      ASSERT(iter%of(), one)
      iter = prev(v%rend(),3_GFTL_SIZE_KIND)
      ASSERT(iter%of(), three)
      
   end subroutine test_prev

#include "parameters/T/undef_derived_macros.inc"
#include "parameters/T/undef_internal.inc"
#include "parameters/T/undef_deque_T.inc"
#include "shared/undef_common_macros.inc"

end module Test_{}_type()DequeRIterator


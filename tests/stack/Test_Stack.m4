changecom()
changequote(`{',`}')
module Test_{}_type()Stack
   use, intrinsic :: iso_fortran_env
   use funit
   use _type()Stack_mod
   ifelse(_type(),{Foo},{use Foo_mod})
   ifelse(_type(),{FooPoly},{use Foo_mod})

#include "_type().inc"
#include "shared/define_common_macros.inc"
#include "test_{}_type().inc"
#include "parameters/T/copy_T_to_stack_T.inc"
#include "parameters/T/copy_stack_T_to_internal_T.inc"
#include "parameters/T/define_derived_macros.inc"

   __T_declare_component__ :: zero
   __T_declare_component__ :: one
   __T_declare_component__ :: two
   __T_declare_component__ :: three

define({ASSERT},{
#if defined(__GFORTRAN__)
ifelse(_type(),{Foo},@assertTrue({$1}=={$2}),
_type(),{FooPoly},@assertTrue({$1}=={$2}),
_type(),{unlimited},@assert_that({$1},is(equal_to({$2}))),
@assertEqual({$1},{$2}))
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
   subroutine test_empty()
     type(Stack) :: v

     v = Stack()
     @assert_that(v%empty(), is(true()))

     call v%push(one)
     @assert_that(v%empty(), is(false()))
   end subroutine test_empty


   @test
   subroutine test_size()
      type(Stack) :: v

      @assert_that(int(v%size()), is(0))
      call v%push(zero)
      @assert_that(int(v%size()), is(1))
      call v%push(zero)
      @assert_that(int(v%size()), is(2))
      
   end subroutine test_size


   @test
   subroutine test_top()
      type(Stack) :: v

      call v%push(one)
      ASSERT(v%top(), one)
      call v%push(two)
      ASSERT(v%top(), two)
   end subroutine test_top



   @test
   subroutine test_pop()
      type(Stack) :: v

      call v%push(one)
      call v%push(two)
      call v%push(three)

      call v%pop()
      
      @assert_that(int(v%size()), is(2))
      ASSERT(v%top(), two)
      call v%pop()
      ASSERT(v%top(), one)

   end subroutine test_pop

   
   @test
   subroutine test_stack_copy()
      type(Stack) :: v1, v2

      call v1%push(one)
      call v1%push(two)
      call v1%push(three)
      v2 = v1
      @assert_that(int(v2%size()), is(3))
      ASSERT(v2%top(), three)
      call v2%pop()
      ASSERT(v2%top(), two)
      call v2%pop()
      ASSERT(v2%top(), one)

   end subroutine test_stack_copy


   @test
   subroutine test_equal()
      type(stack) :: ref, smaller, bigger, different

#if defined(__T__EQ__) || defined(__T_LT__)
      ! [1,2]
      call ref%push(one)
      call ref%push(two)

      ! [1]
      call smaller%push(one)

      ! [1,2,3]
      call bigger%push(one)
      call bigger%push(two)
      call bigger%push(three)

      ! [1,3,2]
      call different%push(one)
      call different%push(three)
      call different%push(two)


      @assert_that(ref == ref, is(true()))
      @assert_that(ref == smaller, is(false()))
      @assert_that(ref == bigger, is(false()))
      @assert_that(bigger == different, is(false()))
#endif      
   end subroutine test_equal

   @test
   subroutine test_not_equal()
      type(stack) :: ref, smaller, bigger, different

#if defined(__T_EQ__) || defined(__T_LT__)
      ! [1,2]
      call ref%push(one)
      call ref%push(two)

      ! [1]
      call smaller%push(one)

      ! [1,2,3]
      call bigger%push(one)
      call bigger%push(two)
      call bigger%push(three)

      ! [1,3,2]
      call different%push(one)
      call different%push(three)
      call different%push(two)


      @assert_that(ref /= ref, is(false()))
      @assert_that(ref /= smaller, is(true()))
      @assert_that(ref /= bigger, is(true()))
      @assert_that(bigger /= different, is(true()))
#endif
   end subroutine test_not_equal

#ifdef __T_LT__
   @test(ifdef=__T_LT__)
   subroutine test_less_than()
      type(stack) :: a, b, c, d

      call a%push(one)
      call a%push(two)

      call b%push(one)
      call b%push(three)

      call c%push(one)
      call c%push(one)

      call d%push(one)
      call d%push(two)
      call d%push(three)


      @assert_that(a < a, is(false()))

      @assert_that(a < b, is(true()))
      @assert_that(b < a, is(false()))

      @assert_that(a < c, is(false()))
      @assert_that(c < a, is(true()))
      
      @assert_that(a < d, is(true()))
      @assert_that(d < a, is(false()))

   end subroutine test_less_than

   @test(ifdef=__T_LT__)
   subroutine test_less_than_or_equal()
      type(stack) :: a, b, c, d

      call a%push(one)
      call a%push(two)

      call b%push(one)
      call b%push(three)

      call c%push(one)
      call c%push(one)

      call d%push(one)
      call d%push(two)
      call d%push(three)


      @assert_that(a <= a, is(true()))

      @assert_that(a <= b, is(true()))
      @assert_that(b <= a, is(false()))

      @assert_that(a <= c, is(false()))
      @assert_that(c <= a, is(true()))
      
      @assert_that(a <= d, is(true()))
      @assert_that(d <= a, is(false()))
   end subroutine test_less_than_or_equal

   @test(ifdef=__T_LT__)
   subroutine test_greater_than()
      type(stack) :: a, b, c, d

      call a%push(one)
      call a%push(two)

      call b%push(one)
      call b%push(three)

      call c%push(one)
      call c%push(one)

      call d%push(one)
      call d%push(two)
      call d%push(three)


      @assert_that(a > a, is(false()))

      @assert_that(a > b, is(false()))
      @assert_that(b > a, is(true()))

      @assert_that(a > c, is(true()))
      @assert_that(c > a, is(false()))
      
      @assert_that(a > d, is(false()))
      @assert_that(d > a, is(true()))
   end subroutine test_greater_than

   @test(ifdef=__T_LT__)
   subroutine test_greater_than_or_equal()
      type(stack) :: a, b, c, d

      call a%push(one)
      call a%push(two)

      call b%push(one)
      call b%push(three)

      call c%push(one)
      call c%push(one)

      call d%push(one)
      call d%push(two)
      call d%push(three)


      @assert_that(a >= a, is(true()))

      @assert_that(a >= b, is(false()))
      @assert_that(b >= a, is(true()))

      @assert_that(a >= c, is(true()))
      @assert_that(c >= a, is(false()))
      
      @assert_that(a >= d, is(false()))
      @assert_that(d >= a, is(true()))
   end subroutine test_greater_than_or_equal

#endif

   @test
   subroutine test_swap()
      type(stack) :: v1, v2

      call v1%push(one)
      call v1%push(two)

      call v2%push(three)

      call v1%swap(v2)

      @assert_that(int(v1%size()), is(1))
      @assert_that(int(v2%size()), is(2))

      ASSERT(v1%top(), three)
      ASSERT(v2%top(), two)
      call v2%pop()
      ASSERT(v2%top(), one)
   end subroutine test_swap
      
#include "parameters/T/undef_derived_macros.inc"
#include "parameters/T/undef_internal.inc"
#include "parameters/T/undef_stack_T.inc"
#include "shared/undef_common_macros.inc"


end module Test_{}_type()Stack

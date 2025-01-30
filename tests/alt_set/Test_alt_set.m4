changecom()
changequote(`{',`}')
module Test_{}_type()_alt_set
   use, intrinsic :: iso_fortran_env
   use funit
   use _type()_alt_set_mod
   ifelse(_type(),{Foo},{use Foo_mod})

#include "_type().inc"
#include "shared/define_common_macros.inc"
#include "test_{}_type().inc"
#include "parameters/T/copy_T_to_set_T.inc"
#include "parameters/T/copy_set_T_to_internal_T.inc"
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
     type(alt_set), target :: s

     s = alt_set()
     @assert_that(s%empty(), is(true()))

     call s%insert(one)
     @assert_that(s%empty(), is(false()))
   end subroutine test_empty

   @test
   subroutine test_max_size()
      type(alt_set), target :: s

      @assert_that(s%max_size(), is(huge(1_GFTL_SIZE_KIND)))
   end subroutine test_max_size


   @test
   subroutine test_size()
      type(alt_set), target :: s

      @assert_that(int(s%size()), is(0))
      call s%insert(zero)
      @assert_that(int(s%size()), is(1))
      call s%insert(one)
      @assert_that(int(s%size()), is(2))
      
   end subroutine test_size

   @test
   subroutine test_size_duplicate_insert()
      type(alt_set), target :: s

      call s%insert(zero)
      call s%insert(zero)
      @assert_that(int(s%size()), is(1))
      
   end subroutine test_size_duplicate_insert

   @test
   subroutine test_count()
      type(alt_set), target :: s

      @assert_that(int(s%count(zero)), is(0))
      call s%insert(zero)
      @assert_that(int(s%count(zero)), is(1))

      @assert_that(int(s%count(one)), is(0))
      call s%insert(one)
      @assert_that(int(s%count(one)), is(1))
      
   end subroutine test_count

   @test
   subroutine test_find_not_found()
      type(alt_set), target :: s
      type(alt_set_iterator) :: iter

      iter = s%find(zero)
      @assert_that(iter == s%end(), is(true()))

   end subroutine test_find_not_found

   @test
   subroutine test_find_found()
      type(alt_set), target :: s
      type(alt_set_iterator) :: iter

      call s%insert(zero)
      call s%insert(one)
      iter = s%find(one)
      @assert_that(iter /= s%end(), is(true()))
      ASSERT(iter%of(), one)

      iter = s%find(zero)
      @assert_that(iter /= s%end(), is(true()))
      ASSERT(iter%of(), zero)
      

   end subroutine test_find_found


   @test
   subroutine test_insert_is_new()
      type(alt_set), target :: s
      logical :: is_new

      call s%insert(zero, is_new=is_new)
      @assert_that(is_new,is(true()))

      call s%insert(one, is_new=is_new)
      @assert_that(is_new,is(true()))

      call s%insert(zero, is_new=is_new)
      @assert_that(is_new,is(false()))
      
   end subroutine test_insert_is_new

   @test
   subroutine test_insert_get_iterator_new()
      type(alt_set), target :: s
      type(alt_set_iterator) :: iter

      call s%insert(zero, iter=iter)
      ASSERT(iter%of(), zero)

      call s%insert(one, iter=iter)
      ASSERT(iter%of(), one)

      call s%insert(zero, iter=iter)
      ASSERT(iter%of(), zero)

      
   end subroutine test_insert_get_iterator_new

   @test
   subroutine test_insert_range()
      type(alt_set), target :: s1, s2


      call s1%insert(zero)
      call s1%insert(one)
      call s1%insert(two)

      call s2%insert(one)
      call s2%insert(s1%begin(),s1%end())
      @assert_that(int(s2%size()), is(3))
      @assert_that(int(s2%count(zero)), is(1))
      @assert_that(int(s2%count(one)), is(1))
      @assert_that(int(s2%count(two)), is(1))
      
   end subroutine test_insert_range

   ! Almost useless test until an implementation is
   ! developed that actually uses the hint
   @test
   subroutine test_insert_with_hint()
      type(alt_set), target :: s
      type(alt_set_iterator) :: iter, hint

      call s%insert(zero)
      call s%insert(one)

      hint = s%find(one)
      print*,__FILE__,__LINE__
      call s%insert(hint, two, iter=iter)
      ASSERT(iter%of(), two)
      
   end subroutine test_insert_with_hint

   @test
   subroutine test_erase_iter()
      type(alt_set), target :: s
      type(alt_set_iterator) :: iter
      
      call s%insert(zero)
      call s%insert(one)
      call s%insert(two)

      ! Delete "one"
      iter = s%find(one)
      iter = s%erase(iter)
      
      @assert_that(int(s%size()), is(2))
      @assert_that(int(s%count(one)), is(0))

   end subroutine test_erase_iter

   @test
   subroutine test_swap()
      type(alt_set), target :: s1, s2

      call s1%insert(one)
      call s1%insert(two)
      call s2%insert(three)

      call s1%swap(s2)

      @assert_that(int(s1%size()), is(1))
      @assert_that(int(s2%size()), is(2))

      @assert_that(int(s1%count(three)), is(1))
      @assert_that(int(s2%count(one)), is(1))
      @assert_that(int(s2%count(two)), is(1))

   end subroutine test_swap


   @test
   subroutine test_ftn_iter()
      type(alt_set), target :: s

      call s%insert(one)
      call s%insert(two)
      call s%insert(three)

      @assert_that('ftn_begin', next(s%ftn_begin()) == s%begin(), is(true()))
      @assert_that('ftn_end', next(s%ftn_end()) == s%end(), is(true()))

   end subroutine test_ftn_iter


#include "parameters/T/undef_derived_macros.inc"
#include "parameters/T/undef_internal.inc"
#include "parameters/T/undef_set_T.inc"
#include "shared/undef_common_macros.inc"
end module Test_{}_type()_alt_set

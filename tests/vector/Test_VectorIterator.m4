changecom()
changequote(`{',`}')
module Test_{}_type()VectorIterator
   use funit
   use, intrinsic :: iso_fortran_env
   use _type()Vector_mod
   ifelse(_type(),{Foo},{use Foo_mod})
   ifelse(_type(),{FooPoly},{use Foo_mod})
   ifelse(_type(),{AbstractBar},{use AbstractBar_mod})

#include "_type().inc"
#include "shared/define_common_macros.inc"
#include "test_{}_type().inc"
#include "parameters/T/copy_T_to_vector_T.inc"
#include "parameters/T/copy_vector_T_to_internal_T.inc"
#include "parameters/T/define_derived_macros.inc"

   __T_declare_component__ :: zero
   __T_declare_component__ :: one
   __T_declare_component__ :: two
   __T_declare_component__ :: three

   __T_declare_component__ :: tmp

define({ASSERT},{
tmp = {$1}
ifelse(_type(),{Foo},@assertTrue(tmp=={$2}),
_type(),{FooPoly},@assertTrue(tmp=={$2}),
_type(),{AbstractBar},@assertTrue(tmp=={$2}),
_type(),{unlimited},@assert_that(tmp,is(equal_to({$2}))),
@assertEqual(tmp,{$2}))
})


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
      type(Vector), target :: v
      type(VectorIterator) :: iter

      call v%push_back(one)
      call v%push_back(two)
      call v%push_back(three)

      iter = v%begin()
     !_ASSERT(iter%of(), one)
      iter = v%end() - 1
     !_ASSERT(iter%of(), three)
   end subroutine test_of

   @test
   subroutine test_of_offset_default()
      type(Vector) :: v
      type(VectorIterator) :: iter

      call v%push_back(one)
      call v%push_back(two)
      call v%push_back(three)

      iter = v%begin() + 1
     !_ASSERT(iter%of(-1), one)
     !_ASSERT(iter%of(+0), two)
     !_ASSERT(iter%of(+1), three)

   end subroutine test_of_offset_default

   @test
   subroutine test_of_offset_size_kind()
      type(Vector) :: v
      type(VectorIterator) :: iter

      call v%push_back(one)
      call v%push_back(two)
      call v%push_back(three)

      iter = v%begin() + 1
     !_ASSERT(iter%of(-1_GFTL_SIZE_KIND), one)
     !_ASSERT(iter%of(+0_GFTL_SIZE_KIND), two)
     !_ASSERT(iter%of(+1_GFTL_SIZE_KIND), three)

   end subroutine test_of_offset_size_kind

   @test
   subroutine test_add()
      type(Vector) :: v
      type(VectorIterator) :: iter

      call v%push_back(one)
      call v%push_back(two)
      call v%push_back(three)

      iter = v%begin()
      call iter%add(2)
     !_ASSERT(iter%of(), three)
      
      call iter%sub(2)
     !_ASSERT(iter%of(), one)

      call iter%add(1_GFTL_SIZE_KIND)
     !_ASSERT(iter%of(), two)

      call iter%sub(1_GFTL_SIZE_KIND)
     !_ASSERT(iter%of(), one)

   end subroutine test_add

   @test
   subroutine test_add_operator()
      type(Vector) :: v
      type(VectorIterator) :: iter, new_iter

      call v%push_back(one)
      call v%push_back(two)
      call v%push_back(three)

      iter = v%begin()
      new_iter = iter + 2
     !_ASSERT(new_iter%of(), three)
      
      new_iter = new_iter - 2
     !_ASSERT(new_iter%of(), one)

      new_iter = iter + 1_GFTL_SIZE_KIND
     !_ASSERT(new_iter%of(), two)

      new_iter = new_iter - 1_GFTL_SIZE_KIND
     !_ASSERT(new_iter%of(), one)

   end subroutine test_add_operator

   @test
   subroutine test_equal()
      type(Vector) :: v
      type(VectorIterator) :: ia, ib

      call v%push_back(one)
      call v%push_back(two)
      call v%push_back(three)

      ia = v%begin()
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
      type(Vector) :: v
      type(VectorIterator) :: ia, ib

      call v%push_back(one)
      call v%push_back(two)
      call v%push_back(three)

      ia = v%begin()
      ib = ia + 1

      @assert_that(ia < ia, is(false()))
      @assert_that(ia < ib, is(true()))
      @assert_that(ib < ia, is(false()))
      @assert_that(ib < ib, is(false()))

   end subroutine test_less_than

   @test
   subroutine test_less_than_or_equal()
      type(Vector) :: v
      type(VectorIterator) :: ia, ib

      call v%push_back(one)
      call v%push_back(two)
      call v%push_back(three)

      ia = v%begin()
      ib = ia + 1

      @assert_that(ia <= ia, is(true()))
      @assert_that(ia <= ib, is(true()))
      @assert_that(ib <= ia, is(false()))
      @assert_that(ib <= ib, is(true()))

   end subroutine test_less_than_or_equal

   @test
   subroutine test_greater_than()
      type(Vector) :: v
      type(VectorIterator) :: ia, ib

      call v%push_back(one)
      call v%push_back(two)
      call v%push_back(three)

      ia = v%begin()
      ib = ia + 1

      @assert_that(ia > ia, is(false()))
      @assert_that(ia > ib, is(false()))
      @assert_that(ib > ia, is(true()))
      @assert_that(ib > ib, is(false()))

   end subroutine test_greater_than

   @test
   subroutine test_greater_than_or_equal()
      type(Vector) :: v
      type(VectorIterator) :: ia, ib

      call v%push_back(one)
      call v%push_back(two)
      call v%push_back(three)

      ia = v%begin()
      ib = ia + 1

      @assert_that(ia >= ia, is(true()))
      @assert_that(ia >= ib, is(false()))
      @assert_that(ib >= ia, is(true()))
      @assert_that(ib >= ib, is(true()))

   end subroutine test_greater_than_or_equal

   @test
   subroutine test_next_method()
      type(Vector) :: v
      type(VectorIterator) :: iter

      call v%push_back(one)
      call v%push_back(two)

      iter = v%begin()
      call iter%next()
      @assert_that(iter == v%end(), is(false()))
      call iter%next()
      @assert_that(iter == v%end(), is(true()))
      
   end subroutine test_next_method

   @test
   subroutine test_prev_method()
      type(Vector) :: v
      type(VectorIterator) :: iter

      call v%push_back(one)
      call v%push_back(two)

      iter = v%end()
      call iter%prev()
      @assert_that(iter == v%begin(), is(false()))
     !_ASSERT(iter%of(), two)

      call iter%prev()
      @assert_that(iter == v%begin(), is(true()))
      
   end subroutine test_prev_method

   @test
   subroutine test_next()
      type(Vector) :: v
      type(VectorIterator) :: iter

      call v%push_back(one)
      call v%push_back(two)

      iter = next(v%begin(),1)
     !_ASSERT(iter%of(), two)

      iter = next(v%begin(),2)
      @assert_that(iter == v%end(), is(true()))

      iter = next(v%begin(),1_GFTL_SIZE_KIND)
     !_ASSERT(iter%of(), two)

      iter = next(v%begin(),2_GFTL_SIZE_KIND)
      @assert_that(iter == v%end(), is(true()))

      
   end subroutine test_next

   @test
   subroutine test_prev()
      type(Vector) :: v
      type(VectorIterator) :: iter

      call v%push_back(one)
      call v%push_back(two)
      call v%push_back(three)

      iter = prev(v%end(),1)
     !_ASSERT(iter%of(), three)
      iter = prev(v%end(),3)
     !_ASSERT(iter%of(), one)
      
      iter = prev(v%end(),1_GFTL_SIZE_KIND)
     !_ASSERT(iter%of(), three)
      iter = prev(v%end(),3_GFTL_SIZE_KIND)
     !_ASSERT(iter%of(), one)
      
   end subroutine test_prev


   @test
   subroutine test_compare_ftn_iter()
      type(Vector) :: v
      type(VectorIterator) :: iter

      call v%push_back(one)
      call v%push_back(two)

      @assert_that(next(v%ftn_begin(),1) == v%begin(), is(true()))
      @assert_that(next(v%ftn_begin(),2) == next(v%begin(),1), is(true()))

      @assert_that(v%ftn_end() == prev(v%end(),1), is(true()))
      @assert_that(prev(v%ftn_end(),1) == prev(v%end(),2), is(true()))
      
   end subroutine test_compare_ftn_iter

   @test
   subroutine test_ftn_next()
      type(Vector) :: v
      type(VectorIterator) :: iter

      call v%push_back(one)
      call v%push_back(two)

      iter = next(v%ftn_begin(),1)
     !_ASSERT(iter%of(), one)

      iter = next(v%ftn_begin(),2)
      @assert_that(iter == v%ftn_end(), is(true()))

      iter = next(v%ftn_begin(),1_GFTL_SIZE_KIND)
     !_ASSERT(iter%of(), one)

      iter = next(v%ftn_begin(),2_GFTL_SIZE_KIND)
      @assert_that(iter == v%ftn_end(), is(true()))

   end subroutine test_ftn_next

   @test
   subroutine test_ftn_prev()
      type(Vector) :: v
      type(VectorIterator) :: iter

      call v%push_back(one)
      call v%push_back(two)
      call v%push_back(three)

      iter = prev(v%ftn_end(),1)
     !_ASSERT(iter%of(), two)
      iter = prev(v%ftn_end(),2)
     !_ASSERT(iter%of(), one)
      
      iter = prev(v%ftn_end(),1_GFTL_SIZE_KIND)
     !_ASSERT(iter%of(), two)
      iter = prev(v%ftn_end(),2_GFTL_SIZE_KIND)
     !_ASSERT(iter%of(), one)
      
   end subroutine test_ftn_prev

#include "parameters/T/undef_derived_macros.inc"
#include "parameters/T/undef_internal.inc"
#include "parameters/T/undef_vector_T.inc"
#include "shared/undef_common_macros.inc"

end module Test_{}_type()VectorIterator


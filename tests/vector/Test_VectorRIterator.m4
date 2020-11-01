changecom()
changequote(`{',`}')
module Test_{}_type()VectorRIterator
   use funit
   use _type()Vector_mod
   ifelse(_type(),{Foo},{use Foo_mod})

#include "_type().inc"
#include "shared/define_common_macros.inc"
#include "test_{}_type().inc"
#include "parameters/T/copy_T_to_vector_T.inc"
#include "parameters/T/copy_vector_T_to_internal_T.inc"
#include "parameters/T/define_derived_macros.inc"

contains

   @test
   subroutine test_of()
      type(Vector) :: v
      type(VectorRIterator) :: iter

      call v%push_back(one)
      call v%push_back(two)
      call v%push_back(three)

      iter = v%rbegin()
      @assert_that(iter%of(), is(equal_to(three)))

      iter = v%rend() - 1
      @assert_that(iter%of(), is(equal_to(one)))

   end subroutine test_of

   @test
   subroutine test_of_offset_default()
      type(Vector) :: v
      type(VectorRIterator) :: iter

      call v%push_back(one)
      call v%push_back(two)
      call v%push_back(three)

      iter = v%rbegin() + 1
      @assert_that(iter%of(-1), is(equal_to(one)))
      @assert_that(iter%of(+0), is(equal_to(two)))
      @assert_that(iter%of(+1), is(equal_to(three)))

   end subroutine test_of_offset_default

   @test
   subroutine test_of_offset_size_kind()
      type(Vector) :: v
      type(VectorRIterator) :: iter

      call v%push_back(one)
      call v%push_back(two)
      call v%push_back(three)

      iter = v%rbegin() + 1
      @assert_that(iter%of(-1_GFTL_SIZE_KIND), is(equal_to(one)))
      @assert_that(iter%of(+0_GFTL_SIZE_KIND), is(equal_to(two)))
      @assert_that(iter%of(+1_GFTL_SIZE_KIND), is(equal_to(three)))

   end subroutine test_of_offset_size_kind

   @test
   subroutine test_add()
      type(Vector) :: v
      type(VectorRIterator) :: iter

      call v%push_back(one)
      call v%push_back(two)
      call v%push_back(three)

      iter = v%rbegin()
      call iter%add(2)
      @assert_that(iter%of(), is(equal_to(one)))
      
      call iter%sub(2)
      @assert_that(iter%of(), is(equal_to(three)))

      call iter%add(1_GFTL_SIZE_KIND)
      @assert_that(iter%of(), is(equal_to(two)))

      call iter%sub(1_GFTL_SIZE_KIND)
      @assert_that(iter%of(), is(equal_to(three)))

   end subroutine test_add

   @test
   subroutine test_add_operator()
      type(Vector) :: v
      type(VectorRIterator) :: iter, new_iter

      call v%push_back(one)
      call v%push_back(two)
      call v%push_back(three)

      iter = v%rbegin()
      new_iter = iter + 2
      @assert_that(new_iter%of(), is(equal_to(one)))
      
      new_iter = new_iter - 2
      @assert_that(new_iter%of(), is(equal_to(three)))

      new_iter = iter + 1_GFTL_SIZE_KIND
      @assert_that(new_iter%of(), is(equal_to(two)))

      new_iter = new_iter - 1_GFTL_SIZE_KIND
      @assert_that(new_iter%of(), is(equal_to(three)))

   end subroutine test_add_operator

   @test
   subroutine test_equal()
      type(Vector) :: v
      type(VectorRIterator) :: ia, ib

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
      type(Vector) :: v
      type(VectorRIterator) :: ia, ib

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
      type(Vector) :: v
      type(VectorRIterator) :: ia, ib

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
      type(Vector) :: v
      type(VectorRIterator) :: ia, ib

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
      type(Vector) :: v
      type(VectorRIterator) :: ia, ib

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
      type(Vector) :: v
      type(VectorRIterator) :: iter

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
      type(Vector) :: v
      type(VectorRIterator) :: iter

      call v%push_back(one)
      call v%push_back(two)

      iter = v%rend()
      call iter%prev()
      @assert_that(iter == v%rbegin(), is(false()))
      @assert_that(iter%of(), is(equal_to(one)))

      call iter%prev()
      @assert_that(iter == v%rbegin(), is(true()))
      
   end subroutine test_prev_method

   @test
   subroutine test_next()
      type(Vector) :: v
      type(VectorRIterator) :: iter

      call v%push_back(one)
      call v%push_back(two)

      iter = next(v%rbegin(),1)
      @assert_that(iter%of(), is(equal_to(one)))

      iter = next(v%rbegin(),2)
      @assert_that(iter == v%rend(), is(true()))

      iter = next(v%rbegin(),1_GFTL_SIZE_KIND)
      @assert_that(iter%of(), is(equal_to(one)))

      iter = next(v%rbegin(),2_GFTL_SIZE_KIND)
      @assert_that(iter == v%rend(), is(true()))

      
   end subroutine test_next

   @test
   subroutine test_prev()
      type(Vector) :: v
      type(VectorRIterator) :: iter

      call v%push_back(one)
      call v%push_back(two)
      call v%push_back(three)

      iter = prev(v%rend(),1)
      @assert_that(iter%of(), is(equal_to(one)))
      iter = prev(v%rend(),3)
      @assert_that(iter%of(), is(equal_to(three)))
      
      iter = prev(v%rend(),1_GFTL_SIZE_KIND)
      @assert_that(iter%of(), is(equal_to(one)))
      iter = prev(v%rend(),3_GFTL_SIZE_KIND)
      @assert_that(iter%of(), is(equal_to(three)))
      
   end subroutine test_prev

#include "parameters/T/undef_derived_macros.inc"
#include "parameters/T/undef_internal.inc"
#include "parameters/T/undef_vector_T.inc"
#include "shared/undef_common_macros.inc"

end module Test_{}_type()VectorRIterator


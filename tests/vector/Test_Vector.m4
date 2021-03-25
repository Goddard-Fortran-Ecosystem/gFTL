changecom()
changequote(`{',`}')
module Test_{}_type()Vector
   use funit
   use _type()Vector_mod
   ifelse(_type(),{Foo},{use Foo_mod})
   ifelse(_type(),{FooPoly},{use Foo_mod})

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
   subroutine test_empty()
     type(Vector) :: v

     v = Vector()
     @assert_that(v%empty(), is(true()))

     call v%push_back(one)
     @assert_that(v%empty(), is(false()))
   end subroutine test_empty

   @test
   subroutine test_max_size()
      type(Vector) :: v

      @assert_that(v%max_size(), is(huge(1_GFTL_SIZE_KIND)))
   end subroutine test_max_size


   @test
   subroutine test_size()
      type(Vector) :: v

      @assert_that(int(v%size()), is(0))
      call v%push_back(zero)
      @assert_that(int(v%size()), is(1))
      call v%push_back(zero)
      @assert_that(int(v%size()), is(2))
      
   end subroutine test_size

   @test
   subroutine test_of_default()
      type(Vector) :: v

      call v%push_back(one)
      call v%push_back(two)
      ASSERT(v%of(1), one)
      ASSERT(v%of(2), two)
      
   end subroutine test_of_default

   @test
   subroutine test_of_size_kind()
      type(Vector) :: v

      call v%push_back(one)
      call v%push_back(two)
      ASSERT(v%of(1_GFTL_SIZE_KIND), one)
      ASSERT(v%of(2_GFTL_SIZE_KIND), two)

   end subroutine test_of_size_kind

   @test
   subroutine test_at_default()
      type(Vector) :: v

      call v%push_back(one)
      call v%push_back(two)
      ASSERT(v%at(1), one)
      ASSERT(v%at(2), two)
      
   end subroutine test_at_default

   @test
   subroutine test_at_size_kind()
      type(Vector) :: v

      call v%push_back(one)
      call v%push_back(two)
      ASSERT(v%at(1_GFTL_SIZE_KIND), one)
      ASSERT(v%at(2_GFTL_SIZE_KIND), two)

   end subroutine test_at_size_kind

   @test
   subroutine test_at_out_of_range()
      type(Vector) :: v
      __T_declare_result__, pointer :: value
      integer :: status

      call v%push_back(one)
      value => v%at(2, rc=status)
      @assert_that(status, is(OUT_OF_RANGE))
      @assert_that(associated(value), is(false()))
      value => v%at(1, rc=status)
      @assert_that(status, is(0))
      @assert_that(associated(value), is(true()))
      
   end subroutine test_at_out_of_range

   @test
   subroutine test_back()
      type(Vector) :: v

      call v%push_back(one)
      ASSERT(v%back(), one)
      call v%push_back(two)
      ASSERT(v%back(), two)
   end subroutine test_back


   @test
   subroutine test_front()
      type(Vector) :: v

      call v%push_back(one)
      ASSERT(v%front(), one)

      call v%push_back(two)
      ASSERT(v%front(), one)
      
   end subroutine test_front


   @test
   subroutine test_reserve()
      type(Vector) :: v

      call v%reserve(5)
      @assert_that(v%capacity() >= 5, is(true()))

   end subroutine test_reserve


   @test
   subroutine test_set_default()
      type(Vector) :: v

      call v%push_back(one)
      call v%set(1, two)
      ASSERT(v%of(1), two)

      call v%resize(5)
      call v%set(5, one)
      ASSERT(v%of(5), one)
      
   end subroutine test_set_default

   @test
   subroutine test_set_size_kind()
      type(Vector) :: v

      call v%resize(5_GFTL_SIZE_KIND)
      call v%set(5_GFTL_SIZE_KIND, one)
      ASSERT(v%of(5_GFTL_SIZE_KIND), one)
      
   end subroutine test_set_size_kind

   @test
   subroutine  test_copy_from_array()
#if !defined(__T_allocatable__) && !(__T_rank > 0)
      type(Vector) :: v
      __T_declare_result__, allocatable :: array(:)

      array = [one, two, three]
      v = array
      @assert_that(int(v%size()), is(3))
      ASSERT(v%of(1), one)
      ASSERT(v%of(2), two)
      ASSERT(v%of(3), three)
      
#endif
   end subroutine test_copy_from_array


   @test
   subroutine test_pop_back()
      type(Vector) :: v

      call v%push_back(one)
      call v%push_back(two)
      call v%push_back(three)
      call v%pop_back()
      
      @assert_that(int(v%size()), is(2))
      ASSERT(v%of(1), one)
      ASSERT(v%of(2), two)

   end subroutine test_pop_back

   
   @test
   subroutine test_vector_fill_default_value()
      type(Vector) :: v
      __T_declare_component__ :: default

      v = Vector(n=3)
      @assert_that(int(v%size()), is(3))
      default = __T_default__

#if __T_type_id__ == __UNLIMITED_POLYMORPHIC__
      ! Forced default to be integer 0
      block
        integer ::i
        do i = 1, 3
           select type(q => v%at(i))
           type is (integer)
              ASSERT(v%at(1), default)
              ASSERT(v%at(2), default)
              ASSERT(v%at(3), default)
           class default
              @assertFail('incorrect default type')
           end select
        end do
      end block
#else
      ASSERT(v%at(1), default)
      ASSERT(v%at(2), default)
      ASSERT(v%at(3), default)
#endif
      
   end subroutine test_vector_fill_default_value

   @test
   subroutine test_vector_fill()
      type(Vector) :: v

      v = Vector(n=3, value=two)
      @assert_that(int(v%size()), is(3))
      ASSERT(v%at(1), two)
      ASSERT(v%at(2), two)
      ASSERT(v%at(3), two)

   end subroutine test_vector_fill

   @test
   subroutine resize_default()
      type(Vector) :: v
      integer :: status

      call v%resize(2, rc=status)
      @assert_that(status, is(0))
      @assert_that(int(v%size()), is(2))

   end subroutine resize_default

   @test
   subroutine resize_default_with_value()
      type(Vector) :: v
      integer :: status

      call v%resize(2, value=three, rc=status)
      @assert_that(status, is(0))
      @assert_that(int(v%size()), is(2))
      ASSERT(v%of(1), three)
      ASSERT(v%of(2), three)

   end subroutine resize_default_with_value

   @test
   subroutine resize_default_with_value_b()
      type(Vector) :: v
      integer :: status

      call v%push_back(one)
      call v%resize(2, value=three, rc=status)
      @assert_that(status, is(0))
      @assert_that(int(v%size()), is(2))
      ASSERT(v%of(1), one)
      ASSERT(v%of(2), three)

   end subroutine resize_default_with_value_b

   ! shrink_to_fit() is allowed to do nothing, but
   ! this implementation does a reallocation to size()
   ! if not same.

   @test
   subroutine test_shrink_to_fit()
      type(Vector) :: v

      call v%push_back(one)
      call v%push_back(two)
      call v%push_back(three)
      call v%pop_back()
      @assert_that(int(v%capacity()) >= 3,is(true()))

      call v%shrink_to_fit()
      @assert_that(int(v%capacity()) == 2,is(true()))
      ! other elements unchanged
      ASSERT(v%of(1), one)
      ASSERT(v%of(2), two)

   end subroutine test_shrink_to_fit

   @test
   subroutine resize_size_kind_with_value()
      type(Vector) :: v
      integer :: status

      call v%resize(2_GFTL_SIZE_KIND, value=three, rc=status)
      @assert_that(status, is(0))
      @assert_that(v%size(), is(2_GFTL_SIZE_KIND))
      ASSERT(v%of(1_GFTL_SIZE_KIND), three)
      ASSERT(v%of(2_GFTL_SIZE_KIND), three)

   end subroutine resize_size_kind_with_value

   @test
   subroutine resize_illegal_size()
      type(Vector) :: v
      integer :: status
      
      call v%resize(-2, rc=status)
      @assert_that(status, is(ILLEGAL_INPUT))

   end subroutine resize_illegal_size

   @test
   subroutine test_erase_one()
      type(Vector) :: v
      type(Vectoriterator) :: iter, next_iter

      call v%push_back(one)
      call v%push_back(two)
      call v%push_back(three)

      iter = v%begin() + 1
      next_iter = v%erase(iter)

      @assert_that(int(v%size()), is(2))
      ASSERT(v%of(1), one)
      ASSERT(v%of(2), three)

      ASSERT(next_iter%of(), three)
      
   end subroutine test_erase_one

   @test
   subroutine test_erase_range()
      type(Vector) :: v
      type(Vectoriterator) :: iter, next_iter

      call v%push_back(one)
      call v%push_back(two)
      call v%push_back(three)
      call v%push_back(one)

      iter = v%begin() + 1
      next_iter = v%erase(iter, iter+2)

      @assert_that(int(v%size()), is(2))
      ASSERT(v%of(1), one)
      ASSERT(v%of(2), one)
      
      ASSERT(next_iter%of(), one)

   end subroutine test_erase_range

   @test
   subroutine test_erase_empty_range()
      type(Vector) :: v
      type(Vectoriterator) :: iter, next_iter

      call v%push_back(one)
      call v%push_back(two)
      call v%push_back(three)
      call v%push_back(one)

      iter = v%begin() + 1
      next_iter = v%erase(iter, iter)

      @assert_that(int(v%size()), is(4))
      @assert_that(next_iter == iter, is(true()))

   end subroutine test_erase_empty_range



   @test
   subroutine test_vector_copy()
      type(Vector) :: v1, v2

      v1 = Vector(n=3, value=two)
      v2 = v1
      @assert_that(int(v2%size()), is(3))
      ASSERT(v2%at(1), two)
      ASSERT(v2%at(2), two)
      ASSERT(v2%at(3), two)

   end subroutine test_vector_copy


   @test
   subroutine test_equal()
      type(vector) :: ref, smaller, bigger, different

#if defined(__T__EQ__) || defined(__T_LT__)
      ! [1,2]
      call ref%push_back(one)
      call ref%push_back(two)

      ! [1]
      call smaller%push_back(one)

      ! [1,2,3]
      call bigger%push_back(one)
      call bigger%push_back(two)
      call bigger%push_back(three)

      ! [1,3,2]
      call different%push_back(one)
      call different%push_back(three)
      call different%push_back(two)


      @assert_that(ref == ref, is(true()))
      @assert_that(ref == smaller, is(false()))
      @assert_that(ref == bigger, is(false()))
      @assert_that(bigger == different, is(false()))
#endif      
   end subroutine test_equal

   @test
   subroutine test_not_equal()
      type(vector) :: ref, smaller, bigger, different

#if defined(__T_EQ__) || defined(__T_LT__)
      ! [1,2]
      call ref%push_back(one)
      call ref%push_back(two)

      ! [1]
      call smaller%push_back(one)

      ! [1,2,3]
      call bigger%push_back(one)
      call bigger%push_back(two)
      call bigger%push_back(three)

      ! [1,3,2]
      call different%push_back(one)
      call different%push_back(three)
      call different%push_back(two)


      @assert_that(ref /= ref, is(false()))
      @assert_that(ref /= smaller, is(true()))
      @assert_that(ref /= bigger, is(true()))
      @assert_that(bigger /= different, is(true()))
#endif
   end subroutine test_not_equal

#ifdef __T_LT__
   @test(ifdef=__T_LT__)
   subroutine test_less_than()
      type(vector) :: a, b, c, d

      call a%push_back(one)
      call a%push_back(two)

      call b%push_back(one)
      call b%push_back(three)

      call c%push_back(one)
      call c%push_back(one)

      call d%push_back(one)
      call d%push_back(two)
      call d%push_back(three)


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
      type(vector) :: a, b, c, d

      call a%push_back(one)
      call a%push_back(two)

      call b%push_back(one)
      call b%push_back(three)

      call c%push_back(one)
      call c%push_back(one)

      call d%push_back(one)
      call d%push_back(two)
      call d%push_back(three)


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
      type(vector) :: a, b, c, d

      call a%push_back(one)
      call a%push_back(two)

      call b%push_back(one)
      call b%push_back(three)

      call c%push_back(one)
      call c%push_back(one)

      call d%push_back(one)
      call d%push_back(two)
      call d%push_back(three)


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
      type(vector) :: a, b, c, d

      call a%push_back(one)
      call a%push_back(two)

      call b%push_back(one)
      call b%push_back(three)

      call c%push_back(one)
      call c%push_back(one)

      call d%push_back(one)
      call d%push_back(two)
      call d%push_back(three)


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
      type(vector) :: v1, v2

      call v1%push_back(one)
      call v1%push_back(two)

      call v2%push_back(three)

      call v1%swap(v2)

      @assert_that(int(v1%size()), is(1))
      @assert_that(int(v2%size()), is(2))

      ASSERT(v1%of(1), three)
      ASSERT(v2%of(1), one)
      ASSERT(v2%of(2), two)
   end subroutine test_swap

   @test
   subroutine test_insert_empty()
      type(vector), target :: v
      type(VectorIterator) :: iter

      iter = v%begin()
      iter = v%insert(iter, one)
      @assert_that(v%size(), is(1_GFTL_SIZE_KIND))
      @assert_that(v%at(1), is(equal_to(one)))
      ASSERT(iter%of(0), one)
      
   end subroutine test_insert_empty

   @test
   subroutine test_insert_before()
      type(vector), target :: v
      type(VectorIterator) :: iter


      call v%push_back(one)
      iter = v%begin()
      iter = v%insert(iter, two)

      @assert_that(v%size(), is(2_GFTL_SIZE_KIND))
      @assert_that(reason='A:', actual=v%at(1), matcher=is(equal_to(two)))
      @assert_that('B:', v%at(2), is(equal_to(one)))
      ASSERT(iter%of(0), two)
      ASSERT(iter%of(1), one)
      
   end subroutine test_insert_before
   
   @test
   subroutine test_insert_middle()
      type(vector), target :: v
      type(VectorIterator) :: iter


      call v%push_back(one)
      iter = v%begin() + 1
      iter = v%insert(iter, two)

      @assert_that(v%size(), is(2_GFTL_SIZE_KIND))
      ASSERT(v%at(1), one)
      ASSERT(v%at(2), two)
      ASSERT(iter%of(0), two)
      ASSERT(iter%of(-1), one)
      
   end subroutine test_insert_middle
   
#include "parameters/T/undef_derived_macros.inc"
#include "parameters/T/undef_internal.inc"
#include "parameters/T/undef_vector_T.inc"
#include "shared/undef_common_macros.inc"


end module Test_{}_type()Vector

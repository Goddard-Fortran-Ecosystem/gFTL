changecom()
changequote(`{',`}')
module Test_{}_type()_alt_setAlgorithms
   use funit
   use, intrinsic :: iso_fortran_env
   use _type()_alt_set_mod
   ifelse(_type(),{Foo},{use Foo_mod})
   ifelse(_type(),{FooPoly},{use Foo_mod})

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

   __T_declare_component__ :: tmp

define({ASSERT},{
tmp = {$1}
ifelse(_type(),{Foo},@assertTrue({tmp}=={$2}),
_type(),{FooPoly},@assertTrue({tmp}=={$2}),
_type(),{unlimited},@assert_that({tmp},is(equal_to({$2}))),
@assertEqual({tmp},{$2}))
})


   integer, save :: counter = 0

contains

   @before
   subroutine setup()

      zero = _zero
      one = _one
      two = _two
      three = _three

   end subroutine setup

#if defined(__T_EQ__)
#define EQ_SUPPORTED
#endif

#ifdef EQ_SUPPORTED
   @test(ifdef=EQ_SUPPORTED)
   subroutine test_find()

      type(alt_set), target :: s
      type(alt_set_iterator) :: iter

      s = alt_set()
      call s%insert(one)
      call s%insert(two)
      call s%insert(three)

      iter = find(s%begin(), s%end(), two)

      ASSERT(iter%of(), two)
   end subroutine test_find
#endif


   ! Synthetic predicates:

   ! Note we cannot use "==" of the contained type as find_if() and
   ! find_if_not() can work with types that do not have "==".
   logical function p1(value)
      __T_declare_dummy__, intent(in) :: value

      counter = counter + 1
      p1 = (counter == 1)

     __UNUSED_DUMMY(value)
   end function p1
   
   logical function p2(value)
      __T_declare_dummy__, intent(in) :: value
      
      counter = counter + 1
      p2 = (counter == 2)
      
     __UNUSED_DUMMY(value)
   end function p2

   subroutine reset_counter()
      counter = 0
   end subroutine reset_counter


   @test
   subroutine test_if()
      type(alt_set), target :: s
      type(alt_set_iterator) :: iter
   
      s = alt_set()
      call s%insert(one)
      call s%insert(two)
      call s%insert(three)


      call reset_counter()
      iter = find_if(s%begin(), s%end(), p2)
      @assert_that(counter, is(2))
      
      call reset_counter()
      iter = find_if(s%begin(), s%end(), p1)
      @assert_that(counter, is(1))

      ! The following line is to avoid compiler warning about not referencing iter
      __UNUSED_DUMMY(iter)
      
   end subroutine test_if

         
   @test
   subroutine test_if_not()
      type(alt_set), target :: s
      type(alt_set_iterator) :: iter
   
      s = alt_set()
      call s%insert(one)
      call s%insert(two)
      call s%insert(three)

      call reset_counter()
      iter = find_if_not(s%begin(), s%end(), p1)
      @assert_that(counter, is(2))

      call reset_counter()
      iter = find_if_not(s%begin(), s%end(), p2)
      @assert_that(counter, is(1))

      ! The following line is to avoid compiler warning about not referencing iter
      __UNUSED_DUMMY(iter)

   end subroutine test_if_not

end module Test_{}_type()_alt_setAlgorithms


#include "parameters/T/undef_derived_macros.inc"
#include "parameters/T/undef_internal.inc"
#include "parameters/T/undef_set_T.inc"
#include "shared/undef_common_macros.inc"


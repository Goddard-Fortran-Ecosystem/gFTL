changecom()
changequote(`{',`}')
module Test_{}_type()VectorAlgorithms
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

#if defined(__T_EQ__)
#define EQ_SUPPORTED
#endif

#ifdef EQ_SUPPORTED
   @test(ifdef=EQ_SUPPORTED)
   subroutine test_find()

      type(Vector), target :: v
      type(VectorIterator) :: iter

      v = Vector()
      call v%push_back(one)
      call v%push_back(two)
      call v%push_back(three)

      iter = find(v%begin(), v%end(), two)

      @assert_that(iter%of(), is(equal_to(two)))
   end subroutine test_find
#endif


end module Test_{}_type()VectorAlgorithms


#include "parameters/T/undef_derived_macros.inc"
#include "parameters/T/undef_internal.inc"
#include "parameters/T/undef_vector_T.inc"
#include "shared/undef_common_macros.inc"


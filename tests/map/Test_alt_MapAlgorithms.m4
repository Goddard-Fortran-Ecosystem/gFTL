changecom()
changequote(`{',`}')
module Test_{}_key(){}_type()alt_MapAlg
   use funit
   use, intrinsic :: iso_fortran_env
   use, intrinsic :: iso_fortran_env
   use funit
   use _key(){}_type()alt_Map_mod
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

#if (defined(__Key_LT__) || defined(__Key_EQ__)) && (defined(__T_LT__) || defined(__T_EQ__))
!  aloha
#    define __map_pair_EQ(lhs,rhs) (lhs==rhs)
#endif
#if defined(__Key_LT__) && defined(__T_LT__)
#    define __map_pair_LT(lhs,rhs) (lhs<rhs)
#endif

   type(Pair) :: zero, one, two, three

   integer, save :: counter = 0

contains

   @before
   subroutine setup()

      zero = Pair(_key_zero,_zero)
      one = Pair(_key_one,_one)
      two = Pair(_key_two,_two)
      three = Pair(_key_three,_three)

   end subroutine setup

#ifdef __map_pair_EQ
   @test(ifdef=__map_pair_EQ)
   subroutine test_find()

      type(Map), target :: m
      type(MapIterator) :: iter

      m = Map()
      call m%insert(one)
      call m%insert(two)
      call m%insert(three)

      iter = find(first=m%begin(), last=m%end(), value=two)
      @assertTrue(iter%of() == two)

   end subroutine test_find
#endif


   ! Synthetic predicates:

   ! Note we cannot use "==" of the contained type as find_if() and
   ! find_if_not() can work with types that do not have "==".
   logical function p1(value)
     type(Pair), intent(in) :: value

      counter = counter + 1
      p1 = (counter == 1)
      
   end function p1
   
   logical function p2(value)
     type(Pair), intent(in) :: value
      
      counter = counter + 1
      p2 = (counter == 2)
      
   end function p2

   subroutine reset_counter()
      counter = 0
   end subroutine reset_counter


   @test
   subroutine test_if()
      type(Map), target :: m
      type(MapIterator) :: iter
   
      m = Map()
      call m%insert(one)
      call m%insert(two)
      call m%insert(three)


      call reset_counter()
      iter = find_if(m%begin(), m%end(), p2)
      @assert_that(counter,is(2))
      
      call reset_counter()
      iter = find_if(m%begin(), m%end(), p1)
      @assert_that(counter,is(1))
      
   end subroutine test_if

         
   @test
   subroutine test_if_not()
      type(Map), target :: m
      type(MapIterator) :: iter
   
      m	= Map()
      call m%insert(one)
      call m%insert(two)
      call m%insert(three)


      call reset_counter()
      iter = find_if_not(m%begin(), m%end(), p1)
      @assert_that(counter,is(2))

      call reset_counter()
      iter = find_if_not(m%begin(), m%end(), p2)
      @assert_that(counter,is(1))
      
   end subroutine test_if_not

end module Test_{}_key(){}_type()alt_MapAlg


#include "parameters/T/undef_derived_macros.inc"
#include "parameters/T/undef_internal.inc"
#include "parameters/T/undef_set_T.inc"
#include "shared/undef_common_macros.inc"

#ifdef __pair_EQ
#    undef __map_pair_EQ
#endif
#ifdef __pair_LT
#    undef __map_pair_LT
#endif

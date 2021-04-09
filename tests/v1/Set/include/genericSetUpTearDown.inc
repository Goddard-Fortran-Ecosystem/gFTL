   subroutine genericSetUp()

      __TYPE_INIT(ONE, _ONE, one_)
!      __TYPE_INIT(ONE_B, _ONE_B, one_b_)
      __TYPE_INIT(TWO, _TWO, two_)
      __TYPE_INIT(THREE, _THREE, three_)
      __TYPE_INIT(FOUR, _FOUR, four_)
      __TYPE_INIT(FIVE, _FIVE, five_)

   end subroutine genericSetUp

   subroutine genericTearDown()

      __TYPE_FREE(ONE)
 !     __TYPE_FREE(ONE_B)
      __TYPE_FREE(TWO)
      __TYPE_FREE(THREE)
      __TYPE_FREE(FOUR)
      __TYPE_FREE(FIVE)

#ifdef __type_allocatable_target
      deallocate(one_)
!      deallocate(one_b_)
      deallocate(two_)
      deallocate(three_)
      deallocate(four_)
      deallocate(five_)
#endif

   end subroutine genericTearDown


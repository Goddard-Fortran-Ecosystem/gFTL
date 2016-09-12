   subroutine genericSetUp()

      __VALUE_INIT(ONE, _value_ONE, one_)
      __VALUE_INIT(TWO, _value_TWO, two_)
      __VALUE_INIT(THREE, _value_THREE, three_)
      __VALUE_INIT(FOUR, _value_FOUR, four_)
      __VALUE_INIT(FIVE, _value_FIVE, five_)


      __KEY_INIT(KEY1, _key_ONE, key1_)
      __KEY_INIT(KEY2, _key_TWO, key2_)
      __KEY_INIT(KEY3, _key_THREE, key3_)
      __KEY_INIT(KEY4, _key_FOUR, key4_)
      __KEY_INIT(KEY5, _key_FIVE, key5_)

   end subroutine genericSetUp

   subroutine genericTearDown()

      __VALUE_FREE(ONE)
      __VALUE_FREE(TWO)
      __VALUE_FREE(THREE)
      __VALUE_FREE(FOUR)
      __VALUE_FREE(FIVE)

      __KEY_FREE(KEY1)
      __KEY_FREE(KEY2)
      __KEY_FREE(KEY3)
      __KEY_FREE(KEY4)
      __KEY_FREE(KEY5)

#ifdef __value_allocatable_target
      deallocate(one_)
      deallocate(two_)
      deallocate(three_)
      deallocate(four_)
      deallocate(five_)
#endif

#ifdef __key_allocatable_target
      deallocate(key1_)
      deallocate(key2_)
      deallocate(key3_)
      deallocate(key4_)
      deallocate(key5_)
#endif

   end subroutine genericTearDown


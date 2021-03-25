!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

      type __iterator
         private

         __declare_element_type, dimension(:), pointer :: elements
         integer(kind=SIZE_KIND) :: currentIndex = -1 ! intentionally invalid value
         
      contains
         
         procedure :: get => __PROC(iter_get)
         procedure :: next => __PROC(iter_next)
         procedure :: previous => __PROC(iter_previous)
         procedure :: __PROC(iter_atDefault)
         generic :: at => __PROC(iter_atDefault)
         procedure :: __PROC(iter_atOffset)
         generic :: at => __PROC(iter_atOffset)
#ifdef SUPPORT_FOR_INT64
         procedure :: __PROC(iter_atOffset_32)
         generic :: at => __PROC(iter_atOffset_32)
#endif
         
         procedure :: __PROC(iter_equal)
         procedure ::  __PROC(not_iter_equal)
         generic :: operator(==) => __PROC(iter_equal)
         generic :: operator(/=) => __PROC(not_iter_equal)
         
         procedure :: __PROC(iter_less)
         procedure :: __PROC(iter_less_equal)
         procedure :: __PROC(iter_greater)
         procedure :: __PROC(iter_greater_equal)
         generic :: operator(<) => __PROC(iter_less)
         generic :: operator(<=) => __PROC(iter_less_equal)
         generic :: operator(>) => __PROC(iter_greater)
         generic :: operator(>=) => __PROC(iter_greater_equal)
         
         procedure :: __PROC(iter_add)
         procedure :: __PROC(iter_subtract)
         generic :: operator(+) => __PROC(iter_add)
         generic :: operator(-) => __PROC(iter_subtract)

#ifdef SUPPORT_FOR_INT64
         procedure :: __PROC(iter_add_32)
         procedure :: __PROC(iter_subtract_32)
         generic :: operator(+) => __PROC(iter_add_32)
         generic :: operator(-) => __PROC(iter_subtract_32)
#endif         
         
      end type __iterator

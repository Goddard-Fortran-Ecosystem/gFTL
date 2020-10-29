!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

#define __QUOTE(x) x

#if (defined _pointer)
#  define _sfx ,pointer
#  define _allocsfx ,pointer
#else
#  define _sfx
#  define _allocsfx ,allocatable
#endif

#if (defined _dim)

#  define _bufentry type(wrapper)
#  define _wrapentry _entry _allocsfx
#  ifdef __INTEL_COMPILER
#    define __SET(x, y) allocate(x, source=y)
#  endif

#elif (defined _string)

#  define _bufentry type(wrapper)
#  define _entry character(len=*)
#  define _wrapentry character(len=:) _allocsfx
#  define _retentry character(len=:)

#elif (defined _pointer)

#  define _bufentry type(wrapper)
#  define _wrapentry _entry, pointer

#endif

#if (defined _pointer)
#  define __SET(x, y) x=>y
#  define __EQ(x, y) sameptr(x, y)
#  ifndef __COMPARE
#    define __COMPARE(x, y) addr(x)<addr(y)
#  endif
#endif


#ifndef _bufentry
#  define _bufentry _entry
#endif

#ifndef _retentry
#  define _retentry _entry
#endif

#ifdef _wrapentry
#  define __GET(x) x%item
      type wrapper
        _wrapentry :: item
      end type wrapper
#endif

#ifndef __GET
#  define __GET(x) x
#endif

#ifndef __SET
#  define __SET(x, y) x=y
#endif

#ifndef __EQ
#  if (defined _logical)
#    define __EQ(x, y) (x.eqv.y)
#  else
#    define __EQ(x, y) (x==y)
#  endif
#endif

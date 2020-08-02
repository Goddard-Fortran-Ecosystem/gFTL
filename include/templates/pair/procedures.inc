#include "parameters/T1/define_derived_macros.inc"
#include "parameters/T2/define_derived_macros.inc"


function __GUARD(get_first)(this) result(first)
   __T1_declare_result__, pointer :: first
   class(__pair), target, intent(in) :: this

   first => this%first
end function __GUARD(get_first)

function __GUARD(get_second)(this) result(second)
   __T2_declare_result__, pointer :: second
   class(__pair), target, intent(in) :: this

   second => this%second
end function __GUARD(get_second)

subroutine __GUARD(swap)(a, b)
   type(__pair), intent(inout) :: a
   type(__pair), intent(inout) :: b

   __T1_declare_component__ :: tmp_first
   __T2_declare_component__ :: tmp_second

   __T1_MOVE__(tmp_first,a%first)
   __T1_MOVE__(a%first,b%first)
   __T1_MOVE__(b%first,tmp_first)
   
   __T2_MOVE__(tmp_second,a%second)
   __T2_MOVE__(a%second,b%second)
   __T2_MOVE__(b%second,tmp_second)
   
end subroutine __GUARD(swap)
         

#include "parameters/T1/undef_derived_macros.inc"
#include "parameters/T2/undef_derived_macros.inc"


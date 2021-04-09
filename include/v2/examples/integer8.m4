changecom()

  use, intrinsic :: iso_fortran_env, only: INT8

#include "types/integer.inc"

#define _T()_KINDLEN(context) (kind=INT8)
#define _T()_kindlen_string "(kind=INT8)"


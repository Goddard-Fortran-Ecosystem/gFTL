changecom()

  use, intrinsic :: iso_fortran_env, only: INT64

#include "types/integer.inc"

#define _T()_KINDLEN(context) (kind=INT64)
#define _T()_kindlen_string "(kind=INT64)"


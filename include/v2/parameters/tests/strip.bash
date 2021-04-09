# This script removes blank lines, CPP output artifacts (starting with "#") and
# Fortran comments.
#  - from Matthew Thompson

#!/bin/bash

INPUTFILE=$1

sed -e '/^#/d' -e '/^!/d' -e 's/^ *$//g' $INPUTFILE | cat -s


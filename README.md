[![Build Status](https://travis-ci.com/Goddard-Fortran-Ecosystem/gFTL.svg?branch=master)](https://travis-ci.com/Goddard-Fortran-Ecosystem/gFTL)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

This package generates containers (Vector, Set, Map, ...) with Fortran interfaces.   It is essentially a brute force analog of C++ STL.

# v2.0 is coming

Starting with the current release, new containers and changed
interfaces are available through the gftl-v2 library.  When v2.0 is
ultimately released, these will become the default, and users that
want to stick with the original interfaces will need to choose
gftl-v1.

## New containers:

* deque
* queue
* stack
* ordered-map
* pair (now a proper container unto itself)
* ptr (to allow proper semantics)

## Other new capabilities:

Preliminary work on algorithms analog.  Currently this is limited
to `find`, `find_if`, and `find_if_not`.  More to come.

## Changes

The changes to the interfaces in 2.0 are generally to improve aligment
with C++ STL.  In particular, interfaces that can throw exceptions in
STL now have an extra "rc" argument that Fortran users should check.
The intent is to provide better documentation in this iteration as
well, with preliminary interface documenation to be found in
`./include/v2/vector.md` and `./include/v2/map.md`.  (Contributions
and corrections very much welcomed on this front!)




# The problem

Fortran only provides one type of container: *array*.  While Fortran
arrays are exemplary for their intended purpose such as numerical
algorithms, they are poorly suited in many other contexts.  Arrays
can be thought of as a particular case of a "container" that holds
multiple entities.  As a container, arrays are well suited for random
access to a fixed number of objects.  (Yes, Fortran arrays are
technically dynamic, but "growing" an array involves multiple steps.)


Many other languages provide additional types of containers that
commonly arise in many contexts.  E.g., a vector (C++ STL) or List
(Java) are _growable_ containers of objects that automatically resize
when required to add a new object.  Another example is that of Map
which allows stores objects as key-value pairs, thereby allowing
retrieval of an object by providing it's key.


# The solution

This package, gFTL, provides a mechanism to easily create robust
containers and associated iterators which can be used within Fortran
applications.  The primary methods are intended to be as close to
their C++ STL analogs as possible.  We have found that these
containers are a powerful productivity multiplier for certain types of
software development, and hope that others find them to be just as useful.

Currently, the following three types of containers are provided.
* Vector (list)
* Set
* Map  (associated array)

Contributions of additional containers are very much welcomed.

## Initial developers

* Tom Clune
* Doron Feldman

# Prerequisites

* CMake 3.0
* GNU m4 (must be in path as "gm4" or "m4")

# Related package

It is worth noting that there is a similar package
[FTL](https://github.com/robertrueger/ftl) which may be of interest.
gFTL was developed independently of FTL, but was not open-sourced in
time to claim the cooler name.


## Quick overview of gFTL vs FTL

I expect this section to grow a bit more after the authors of the two
packages have had time to discuss.  It is highly desired that this
section be factually correct.

### Similarities

* Both packages use the preprocessor that is built-in to essentially
  all modern Fortran compilers.

### Differences

* Naming conventions for gFTL are  closer to C++ STL.



# Request support

If you have any questions, please contact:

* Tom Clune  (Tom.Clune@nasa.gov)



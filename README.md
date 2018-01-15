# The problem

Fortran only provides one type of container: *array*.  While Fortran
arrays are exemplary for their intended purpose such as numerical
algorinthms, they are poorly suited in many other contexts.  Arrays
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

* Naming conventions for gFTL are much closer to C++ STL.



# Request support

If you have any questions, please contact:

* Tom Clune  (Tom.Clune@nasa.gov)



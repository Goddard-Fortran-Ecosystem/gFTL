# Change Log

## Unreleased

## [1.5.1] - 2021-09-28

### Fixed

- RECURSIVE needed in some procedures for advanced use cases.


## [1.5.0] - 2021-09-22

This will be the final (non bugfix) release of version 1.

Future version 2 releases will break backwards compatibility by making
the v2 containers default, but v1 containers will still be accessible with
minimal changes.

Documentation for v2 containers should be completed soon, but as
always, the comparable C++ STL documentation is generally quite close
modulo Fortran syntax.


### Fixed
- Added support for compiling on Windows with Intel compiler.
- Corrected uninitialized value problem with v2 maps.  Detected
  by NAG with aggressive debug flags.
- Corrected logic for default value for vectors of (polymorphic)
  abstract objects.
- Corrected logic for standard GFortran workarounds for fHamcrest tests.

### Changed

- Simplified various expressions involving type operations.   Intel 2021.1 was choking
  on an external use case but seems happier if intemediate variables are introduced
  within expressions involving user defined `==` and `<`.   Some attempt was made to use
  `ASSOCIATE` instead, but NAG 7.0 had trouble with one of those.    A reproducer for
  the Intel problem has been created and will be submitted.
- Set container now uses ALLOCATABLE for children rather than pointers.   This makes
  some internal algorithms a bit painful to implement, but avoids the need for explicit
  FINAL methods which appear to expose buggy compilers.  (Hard to isolate though.)

### Deleted

- Deleted altSet support in v2 containers.  This was always intendend
  as a workaround for limitations in FINAL implementation by various
  compilers.  But v2 set now uses allocatable containers and sidesteps
  this.  (Also, altset was never completed in v2 containers anyway.)

## [1.4.1] - 2021-04-09

### Fixed

- Last minute fixes for gfortran missed a duplicate type declaration
  caught by NAG.

### Added

- enabled double precision complex in v2 interfaces.

## [1.4.0] - 2021-04-09

### Added

- Entirely revamped interfaces as prelude to v2.0.   Generally much closer to C++ STL than
  previous releases.   v1 interfaces are still the default and will be maintained after v2.0.
- Includes new containers:
  . ptr
  . deque
  . stack
  . queue
  . ordered-map
- Beginnings of real documentation - see `./include/v2/vector/vector.md` and
  `./include/v2/map/map.md`
  

## [1.3.1] - 2021-02-06

### Changed

### Fixed

- Some conflicts among GFE build chains related to previous version.

## [1.3.0] - 2020-12-07

- gFTL now uses CMake namespaces.   Upstream projects should now link with
  'GFTL::gftl` instead of `gftl`.  Technically a backward incompatibility, but
  does not seem serious enough to warrant a major release.

- Export CMake targets to make it easier to consume.    (Contrib)

## [1.2.7] - 2020-08-25

###Changed
- Undoing previous commit.  Caused insidious downstream issues with 18.0.5.
  Better to just kludge the failing test and move on.

## [1.2.6] - 2020-08-24


### Fixed
- Blocked installation of *Foo*.inc include files that are
  only used for testing.

- Reintroduced workarounds for older ifort and gfortran compilers.
  Annoyingly the workaround for one breaks the other.
  
=======
## [2.0.0] - ????

## Fixed
- +/- operators on vector reverse iterators were incorrectly implemented in v1.

## Changed
- Changed location of GFTLConfig.cmake

    Now installed in a more conventional location:
    ```./lib/cmake/GFTL/GFTLConfig.cmake```
    Previous location was really a windows convention (first attempt).
    Also now using
      . ```configure_package_config_file()
      . ```write_basic_package_version_file()```

- Moved ./cmake_utils to ./cmake
- Relocated config related files to ./cmake
- Added namespace ```gFTL::``` for externa project use  
- Cleaned up CMake logic in include dirs.
- Only search for testing if Fortran compiler is detected.  gFTL does
  not require a Fortran compiler for installation, but Fortran is
  required to build the tests.  Previously, gFTL artificially included
  Fortran in the project LANGUAGES option to sidestep the issue.
- Moved legacy include to include-v1
- Changed capitalization of installation from GFTL to gFTL.
  Users should now use ```find_package(gFTL)```
- Vector template
  . relational operators are module procedures rather than class methods
  . arithmetic operators are module procedures rathen than class methods
  . introduced a few iterator module procedures: advance, begin, distance, end, next, previous

## Added
- Support for type names.

  This is to allow useful diagnostics via DTIO.
  

>>>>>>> ad6bb41... Changed location of GFTLConfig.cmake
## [1.2.5] - 2020-04-06

### Fixed
 - eliminated stray <tab> in source code; was generating annoying
   warnings in some compilers
	
## [1.2.4] -  2019-12-19
- fixes for CMakeList
  - removed incorrect parens in conditionl
  - added Fortran to PROJECT - necessary for recent change in pFUnit.

## [1.2.3] -  2020-01-06
- Workaround for PGI compiler - does not like "set" as a derived type name.

## [1.2.2] - 2019-11-15
- bugfix for workaround in v1.2.1; some use cases were not
  deallocating structure components prior to reallocation.
	
## [1.2.1] - 2019-11-07
- added workaround for memory leak detected with Intel 18 compiler

## [1.2.0] - 2019-09-01
- updated to use pFUnit 4.0 for tests
- started maintaining a change log


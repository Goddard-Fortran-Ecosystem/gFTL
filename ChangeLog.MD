# Change Log

## Unreleased

<<<<<<< HEAD
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


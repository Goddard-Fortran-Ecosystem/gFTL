# Change Log

## Unreleased

### Fixed

- Added missing TARGET attribute in map::erase() procedures.  When
  used with NAG compiler, would result in temporary objects with
  dangling pointers. Added new test to cover this case.

## [1.14.0] - 2024-07-09

### Fixed

- Change use of `spread` in `include/v2/parameters/define_derived_macros.m4` to `reshape` to avoid NVHPC issue

### Added

- Enabled feature that allows pointers into container objects to remain valid when container grows/shrinks.  This ability is already baked into the polymorphic case where `MOVE_ALLOC()` is used.  Now if one uses `#define T_deferred` gFTL will also use a wrapper type with an allocatable component and `MOVE_ALLOC()` under the hood.

### Changed

- Update CI to remove `macos-12`, add `macos-14` and `ubuntu-24.04`

## [1.13.0] - 2024-03-20

### Fixed

- Implemented workarounds for ifx compiler and v1 interfaces. v2 tests were fine.
  1. Removed previous workaround for older Intel compilers that did not support polymorphic assignment.
  2. Introduced workaround for comparing polymorphic pointers for Set and altSet

### Changed

- Removed `macos-11` from CI, added `macos-13`
- Added `-quiet` flag for NAG compiler
- Removed stray print statements in tests.

## [1.12.0] - 2024-03-03

### Added

- Fujitsu compiler support

### Fixed

- (#211) Fixed bug in implementation of erase(first, last) for vector & deque containers.  When the range was empty, then some cases would erronously call `MOVE_ALLOC(x,x)` which is illegal. 

## [1.11.0] - 2023-11-28

### Added

- Introduced new preprocessing options to disable override of `assignment(=)` in v2 map and set templates.   This is to workaround a case where intel compiler is producing erroneous results.   Disabling everywhere is too risky.

### Changed

- Behavior of `at(key)` (without rc) now will not extend the map.   This change may be reverted if it breaks any downstream projects.
- Remove Ubuntu 20 and gfortran-9 from CI

### Fixed

- Add `-check nouninit` for Intel LLVM to work around [`ifx` bug](https://github.com/HPC-Bugs/reproducers/tree/main/compiler/Fortran/ifx/allocatable).

## [1.10.0] - 2023-04-13

### Added

- Added `IntelLLVM.cmake` file as a copy of `Intel.cmake` to support the LLVM Intel compiler frontends
  (Note - this has not been tested due to lack of system access.)
  
## [1.9.0] - 2023-04-12

### Added

- Added Fortran-friendly iterator factories (and tests) to

  `ftn_begin()` and `ftn_end()` have been added to vector, set, map,
  and ordered_map templates.  `ftn_begin()` points to just-before the
  1st element of a container, while `ftn_end()` points to the last
  element.   This allows the `next()` invocation to be at the start of the loop which
  is less error prone in the presence of `CYCLE` statements.   Example usage:
  
  ```f90
  type(Vector) :: v
  type(VectorIterator) :: iter
  ...
  associate (e => v%ftn_end())
     iter = v%ftn_begin()
     do while (iter /= e)
        call iter%next()
        ...
        if (<cond>) cycle ! does the right thing
        ...
     end do
  end associate
  ```

## [1.8.3] - 2023-01-27

### Fixed

- Missing `KIND=` on `size()` procedure.  Gave incorrect response for large containers.  Only affects V1 containers.

## [1.8.2] - 2023-01-23

### Fixed

- Fix issues with GNU Make builds

## [1.8.1] - 2022-06-28

### Fixed

- Prevented some macro redefinitions in `v1/map.inc` that caused warning messages and broke documentation tool on downstream projects.


## [1.8.0] - 2022-05-31

### Changed

- Interfaces (intents) to map::at() and ordered_map::at().  The self object should be `INTENT(IN)` for these interfaces.

- Updated GitHub Actions
  - OSs
    - Remove macos-10.15
    - Add ubuntu-22.04 and macos-12
  - Compilers
   - Removed gfortran-8
   - Added gfortran-11
   - Added gfortran-12 (for ubuntu-22.04)

### Fixed

- Fixed bug in `vector::insert_range()` (needs a unit test)

- Added `TARGET` to some dummy arguments to prevent NAG from doing copy-in / copy-out.  Seems to  have been causing a memory corruption issue with the yaFyaml package, but might indicate a bug
  in yaFyaml (or gFTL).  

- Fixed non-standard-conforming mismatched integer KIND in deque implementation.

- Fixed misspelling of SUCCESS

### Removed

- Various unused local variables that were causing annoying compiler warnings.

- Reactivated unit tests (involving Foo) that were deactivated as a workaround for some NAG 7.1 compiler releases.


## [1.7.2] - 2022-05-08

### Fixed

- Patched (again) such that the assignment operator for Set invokes a deep copy.  Previously I mistakenly thought that since the data structure now uses ALLOCATABLE components that Fortran intrinsic assignment would suffice.  But forgot that the "parent" component is still a pointer.  Failures from this assumption were subtle as often parents would point to   correctly looking copies outside the intended data structure.

- Missing RECURSIVE declarations on some procedures.  The issue arises for YAML data structures where a map may contain items of the same map type and similarly for vectors.

### Added

- A verify() method was added to several containers to aid in debugging some of the problems mentioned above.   Users of gFTL should not use this method - its interface may change and the procedure may even disappear in later releases.

## [1.7.1] - 2022-04-24

### Fixed

- Corrected previous commit that eliminated the non intrinsic
  assignment for set, map, and `ordered_map` containers.  Just had not
  thought through why these are needed: internal pointers are not
  correctly established by intrinsic assignment.  I was too focused on
  the nice property that allocatable components ar correctly
  _deallocated_.   Use cases would sometimes work, because the compiler
  was avoiding copies when using constructors. 
p

## [1.7.0] - 2022-04-08

### Changed

- Eliminated type-bound overload of `assignment(=)` for set, map, and ordered_map.  It was found that
  it caused problems with ifort 2021.5.0 and not needed by other compilers after other recent changes.

- Also made deep-copy RECURSIVE, as this is needed in the yaFyaml use
  case.  RECURSIVE is of course now default in Fortran, but not all compilers
  have implemented this consistently at this time.

## [1.6.1] - 2022-03-30

### Fixed

- Missing RECURSIVE on methods in vector template.  This is necessary
  for some use cases involving recursive data structures.  In particular
  sequences-of-sequences in yaFyaml.  GFortran has not yet implemented
  default RECURSIVE, which is what actually makes this necessary.  (So not
  technically a bug, but ...)
  
## [1.6.0] - 2022-03-16

### Added

- NVHPC compiler support

### Removed

- Removed `PGI.mk` and `NAG.mk` files which seem to be unneeded cruft

## [1.5.5] - 2022-03-07


### Changed

- On Windows M4 program is now downloaded from SourceForge during CMake configuration
  if it is not found.

### Fixed

- Some procedures for `map` and `ordered_map` had not been implemented and are now added in.  This was an oversight and thus being treated as a bug rather than a feature. 


## [1.5.4] - 2022-01-10

### Fixed

- Minor bug in Pair template that is exposed under certain combinations
  of key/value types.   If key has "==" but "value" does not, then some
  local variable declarations are skipped that are needed.
  
- Declarion of Ptr argument in Pointer template.   Still not ready 
  for production use though.
  

## [1.5.3] - 2021-11-16

### Fixed

- Fixed typo in `#undef` that was creating noisy compiler warnings.  (@ZedThree)

## [1.5.2] - 2021-11-14

### Fixed

- Bug in set::erase() where certan scenarios resulted in
  an incorrect (and inconsistent) state.
  
- Missing quotes in Intel.cmake for Windows builds.  Recent CMake versions
  detect this even in Linux builds.
  


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


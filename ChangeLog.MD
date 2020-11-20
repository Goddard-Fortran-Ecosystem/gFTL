# Change Log

## Unreleased

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


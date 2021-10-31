# gge 1.7

Fix a note about LazyData.

## Test environments & results

* local R 4.1.0 on Windows 10
* WinBuilder R-devel
* WinBuilder R-release

## R CMD check results

OK, except ONE platform on Rhub claims that `testthat` has an error, but it does not actually show any errors.  Weird.
    

# gge 1.6

Moved rgl to Suggests at request of Brian Ripley.

## Test environments & results

* local R 4.0.3 on Windows 10
* Rhub Fedora Linux, R-devel, clang, gfortran
* WinBuilder R-devel

## R CMD check results

No ERRORs or WARNINGs or NOTEs
 
## Reverse dependencies

No ERRORs in agridat, GGEBiplots.


# gge 1.5

## test environments

* local R 4.0.2 on Windows 10
* Rhub Windows Server 2008 R=devel
* Rhub Ubuntu Linux R-release
* Rhub Fedora Linux R-devel

## R CMD check results

No ERRORs, or WARNINGs.

1 NOTE. (This word is correct)
Possibly mis-spelled words in DESCRIPTION:
Laffont (12:9)
 
## Reverse dependencies

No ERRORs in agridat, GGEBiplots.


# gge 1.4

## test environments

* local R 3.5.0 on Windows 7
* win-builder release
* win-builder devel

## R CMD check results

There were no NOTEs, ERRORs, or WARNINGs.

## Reverse dependencies

No ERRORs in agridat, GGEBiplots.

# gge 1.3

## test environments

* local R 3.4.2 on Windows 7
* win-builder release
* win-builder devel

## R CMD check results

There were no ERRORs, or WARNINGs.

There is one NOTE:

Possibly mis-spelled words in DESCRIPTION:
  Biplots (2:46)
  GGB (10:5)
  GGE (9:33)
  Genotype (2:8, 2:22)
  biplots (9:21)
  
# gge 1.2

## test environments

* local R 3.4.0 on Windows 7
* win-builder release
* win-builder devel

## R CMD check results

There were no ERRORs, or WARNINGs.

There is one NOTE:

Possibly mis-spelled words in DESCRIPTION:
  Biplots (2:46)
  GGB (10:5)
  GGE (9:33)
  Genotype (2:8, 2:22)
  biplots (9:21)
  
## Downstream dependencies

None.

# gge 1.1

## test environments

* local R 3.3.1 on Windows 7
* win-builder release
* win-builder devel

## R CMD check results

There were no ERRORs, or WARNINGs.

There is one NOTE:

Possibly mis-spelled words in DESCRIPTION:
  Biplots (2:46)
  GGB (10:5)
  GGE (9:33)
  Genotype (2:8, 2:22)
  biplots (9:21)
  
## Downstream dependencies

None.

# gge 1.0

This is a new package submission.  The functions in this package are being
split off from the 'agridat' package so that 'agridat' can be a data-only
package.

## test environments

* local R 3.2.3 on Windows 7
* win-builder (devel & release)

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.


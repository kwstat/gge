## Todo

New argument `focus` for Different Singular Value Partitionings, including GH, JK, SQRT, HJ.

New function `ggbiplot()` function using `ggplot2` graphics to draw the biplot.

Average Environment Coordinate

Bootstrap testing for PCs (Forkman 2019 paper)

Bootstrap conf int

# gge 1.6 ()

Brian Ripley wrote: "The future of OpenGL is uncertain (except on macOS, where it has no future).  So it seems reasonable to require `rgl` only when essential to the package.  These packages have it in Depends/Imports but seem not to actually call it in their checks (established using a fake install).  It is possible that the sole purpose of the package might be to do interactive visualizations which are not checked, but that seems not to be the case here.  We noticed calls to rgl functions in \dontrun{}, but they would better be conditioned by if(interactive()) (see 'Writing R Extensions'). Please move rgl to Suggests and use conditionally (see §1.1.3.1 of 'Writing R Extensions') at the next package update.""

* Moved rgl to Suggests. Cannot use `require(rgl)` because that crashes R for some users, so use `if("rgl" %in% installed.packages() )` to check for installation and then call functions `rgl::open3d()`

# gge 1.5 (20 Jul 2020)

* Please use `gge(data,formula)` instead of `gge(formula,data)`.

* New argument `ggb=TRUE` to request construction of GGB biplot.

# gge 1.4 (15 May 2018)

* Use `cex.gen=0` to omit genotype names.

* On some Windows machines, `library(rgl)` crashes R, perhaps because of a dll conflict with Windows. Removed `@import rgl` so that `rgl` is not loaded by default, and now `biplot3d` uses calls like `rgl::text3d`.

# gge 1.3 (14 Dec 2017)

* The `nipals()` function using C++ code has been removed.

* The `rnipals()` function has been removed.

* The `gge` package now imports the `nipals` package, which is new.

# gge 1.2 (25 May 2017)

* New function `nipals()` for finding principal components using C++.  Code from `pcaMethods` package.

* New function `rnipals()` for finding principal components in R.

* New function `biplot3d()` to draw 3d biplots using `rgl` package.

* Modifed `main`, `subtitle`, `xlab`, `ylab` arguments to allow removal.

* Changed `title` argument to `main` for consistency with other packages.

* Now using `testthat` and `covr` packages.

* Added package logo on github.

# gge 1.1 (8 Oct 2016)

* Added `zoom.gen` and `zoom.env` arguments to `biplot()` for  M.Zoric.

* Moved tests to `tests/gge_tests.R`

# gge 1.0 (14 Dec 2015)

* Package `gge` is split off from `agridat` package.

* Added `origin`, `hull` arguments to `biplot()`.

# gge 0.1 (2013)

* Added `gge()` to agridat package.

# gge 0.0 (May 2004)

* Created function `gge()` to fit and plot GGE biplots.

## A history of NIPALS functions

1. (2007) Created `nipals()` based on `pcaMethods::nipalsPca()`.  Modified the function for faster execution and submitted a patch back to `pcaMethods`.

2. (2010) Henning Redestig created a C++ version of NIPALS for the `pcaMethods` package.

3. (2017) The `gge::nipals()` R function is re-named `rnipals()`, and a new `nipals()` function is created, based on the C++ code in `pcaMethods`. Released gge version 1.2.

4. (2017) Discovered that `mixOmics::nipals()` is a pure R function that is faster than the C++ version, so `gge::nipals()` was re-written into a pure R function. The C++ version was removed from the `gge` package. 

5. The `gge::nipals` function is moved to a new package, `nipals::nipals`. The function is optimized for performance, improved to better handle missing values and to orthogonalize the principal components. 

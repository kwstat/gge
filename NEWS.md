
Todo: New argument `focus` for Different Singular Value Partitionings, including GH, JK, SQRT, HJ.

Todo: New function `ggbiplot()` function uses `ggplot2` graphics to draw the biplot.

Todo: AEC

# gge 1.3 - unpublished

The `nipals()` function using C++ code has been removed. The `rnipals()` function has been removed. 

The `gge()` function now imports the `nipals` package.

## A history of NIPALS functions

2007. Created `nipals()` based on `pcaMethods::nipalsPca()`.  Modified the function for faster execution and submitted a patch back to `pcaMethods`.

2010. Henning Redestig created a C++ version of NIPALS for the `pcaMethods` package.

2017. The `gge::nipals()` R function is re-named `rnipals()`, and a new `nipals()` function is created, based on the C++ code in `pcaMethods`. Released gge version 1.2.

2017. Based on tests, it was discovered that `mixOmics::nipals()` is a pure R function that is faster than the C++ version, so `gge::nipals()` was re-written into a pure R function. The C++ version was removed from the `gge` package. 

The `gge::nipals` function is moved to a new package, `nipals::nipals`. The function is optimized for performance, improved to better handle missing values and to orthogonalize the principal components. 

# gge 1.2 - May 2017

New function `nipals()` for finding principal components using C++.  Code from `pcaMethods` package. (GPL-3 license.)

New function `rnipals()` for finding principal components in R.

New function `biplot3d()` to draw 3d biplots using `rgl` package.

Modifed `main`, `subtitle`, `xlab`, `ylab` arguments to allow removal.

Changed `title` argument to `main` for consistency with other packages.

Now using `testthat` and `covr` packages.

Added package logo on github.

# gge 1.1 - Oct 2016

Added zoom.gen, zoom.env arguments to `biplot()`for M.Zoric.

Moved tests to tests/gge_tests.R

# gge 1.0 - Dec 2015

Package forked and split off from agridat package.

Added origin/hull arguments to `biplot()`.

# gge 0.1 - 2013

Added `gge()` to agridat package.

# gge 0.0 - May 2004

Created function `gge()` to fit and plot GGE biplots.


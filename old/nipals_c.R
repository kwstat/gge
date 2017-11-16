#' PCA by non-linear iterative partial least squares
#'
#' Used for finding principal components of a numeric matrix.  Components
#' are extracted one a time.  Missing values in the matrix are allowed.
#' The \code{nipals} function is coded in C++.
#' The \code{rnipals} function is coded in R.
#' The algorithm computes x = TV', where T is the 'scores' matrix and V is
#' the 'loadings' matrix.
#' 
#' @name nipals
#' 
#' @param x Numerical matrix
#' 
#' @param maxcomp Maximum number of principal components to extract.
#'
#' @param maxiter Maximum number of NIPALS iterations to perform.
#'
#' @param propvar The proportion of variance that should be explained by the
#' returned principal components. If propvar < 1, then \code{maxcomp} is ignored.
#'
#' @param tol Default 1e-6 tolerance for testing convergence of the algorithm.
#'
#' @param center If TRUE, do center columns.
#'
#' @param scale. If FALSE, do not scale columns.
#'
#' @param ... Only used for passing through arguments.
#' 
#' @return A list with components \code{scores}, \code{rotation}, 
#' \code{completeObs}, \code{maxcomp}, \code{center}, \code{scale}, 
#' \code{sdev}, \code{R2}, \code{eval}, \code{propvar}.
#' 
#' @references
#' Wold, H. (1966) Estimation of principal components and
#' related models by iterative least squares. In Multivariate
#' Analysis (Ed., P.R. Krishnaiah), Academic Press, NY, 391-420.
#' 
#' @examples 
#' B <- matrix(c(50, 67, 90, 98, 120,
#'               55, 71, 93, 102, 129,
#'               65, 76, 95, 105, 134,
#'               50, 80, 102, 130, 138,
#'               60, 82, 97, 135, 151,
#'               65, 89, 106, 137, 153,
#'               75, 95, 117, 133, 155), ncol=5, byrow=TRUE)
#' rownames(B) <- c("G1","G2","G3","G4","G5","G6","G7")
#' colnames(B) <- c("E1","E2","E3","E4","E5")
#' B = scale(B, scale=FALSE) # column-centered
#' dim(B) # 7 x 5
#' p1 <- nipals(B, maxcomp=5, center=FALSE)
#' dim(p1$scores) # 7 x 5
#' dim(p1$rotation) # 5 x 5
#' round(p1$scores %*% t(p1$rotation) - p1$completeObs,2) # 0
#' p2 <- rnipals(B, maxcomp=5, center=FALSE)
#' round(p2$scores %*% t(p2$rotation) - p2$completeObs,2) # 0
#' 
#' @author Kevin Wright (R version), Henning Redestig (C++ version).
#' 
#' @export
nipals <- function(x, maxcomp=min(nrow(x), ncol(x)),
                   maxiter=5000,
                   tol=1e-6, propvar=1,  
                   center=TRUE, scale.=FALSE, ...) {
  
  x <- as.matrix(x)
  x.orig <- x # Save x for replacing missing values
  
  x <- scale(x, center=center, scale=scale.)
  cen <- attr(x, "scaled:center")
  sc <- attr(x, "scaled:scale")
  if (any(sc == 0))
    stop("cannot rescale a constant/zero column to unit variance")
  
  # Check for a column/row with all NAs
  col.count <- apply(x, 2, function(x) sum(!is.na(x)))
  if(any(col.count==0)) warning("At least one column is all NAs")
  row.count <- apply(x, 1, function(x) sum(!is.na(x)))
  if(any(row.count==0)) warning("At least one row is all NAs")
  
  # danger! scale() adds additional attributes to 'x' which confuses the C code
  # and over-writes some memory. Remove these attributes before handing to C
  mat=x
  attr(mat, "scaled:center") <- NULL
  attr(mat, "scaled:scale") <- NULL
  nipRes <- .Call("gge_Nipals", mat,
                  params=list(maxcomp=maxcomp,
                              propvar=propvar,
                              tol=tol,
                              maxiter=maxiter),
                  PACKAGE="gge")
  
  scores <- nipRes$scores
  loadings <- nipRes$loadings
  R2cum <- nipRes$R2cum
  
  # un-cumulate R2
  R2 <- c(R2cum[1], diff(R2cum))
  
  # eigen values
  eval = apply(scores, 2, function(x) sum(x*x))
  
  # re-construction of x using maxcomp principal components
  fitted.values <- scores[ , 1:maxcomp] %*% t(loadings[ , 1:maxcomp])
  if(scale.) fitted.values <- fitted.values * attr(x, "scaled:scale")
  if(center) fitted.values <- fitted.values + attr(x, "scaled:center")
  
  # replace missing values in the original matrix with fitted values
  completeObs <- x.orig
  completeObs[is.na(x.orig)] <- fitted.values[is.na(x.orig)]
  
  # prepare output
  rownames(scores) <- rownames(x)
  colnames(scores) <- paste("PC", 1:ncol(scores), sep="")
  rownames(loadings) <- colnames(x)
  colnames(loadings) <- paste("PC", 1:ncol(loadings), sep="")
  out <- list(scores = scores, rotation = loadings,
              completeObs = completeObs,
              maxcomp = maxcomp,
              center=if(is.null(cen)) FALSE else cen,
              scale=if(is.null(sc)) FALSE else sc,
              sdev=apply(scores, 2, sd), # needed for prcomp print method
              R2 = R2,
              eval=eval,
              propvar = propvar)
  class(out) <- c("nipals","prcomp")
  return(out)
}


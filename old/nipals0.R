#' PCA by non-linear iterative partial least squares, coded in R.
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
#' @param verbose FALSE. If TRUE, show diagnostic output.
#'
#' @return A list with components.
#'
#' @author Kevin Wright
#' 
#' @export
rnipals <- function(x, maxcomp=min(nrow(x), ncol(x)-1),
                    maxiter=5000,
                    tol=1e-6, propvar=1,
                    center=TRUE, scale.=FALSE, verbose=FALSE) {
  # Calculate principal components using NIPALS
  # Author: Kevin Wright
  
  # A nice summary of NIPALS is here:
  # http://statmaster.sdu.dk/courses/ST02/module06/index.html
  
  # This currently produces an object of class 'prcomp', but maybe it
  # would be better to return objects the same way that svd does???
  
  x <- as.matrix(x)
  x.orig <- x # Save x for replacing missing values
  x <- scale(x, center=center, scale=scale.)
  cen <- attr(x, "scaled:center")
  sc <- attr(x, "scaled:scale")
  if (any(sc == 0))
    stop("cannot rescale a constant/zero column to unit variance")
  
  # sum(NA, na.rm=TRUE) is 0, but we want NA
  sum.na <- function(x){ ifelse(all(is.na(x)), NA, sum(x, na.rm=TRUE))}
  
  n.missing <- sum(is.na(x))
  
  # Check for a column/row with all NAs
  col.count <- apply(x, 2, function(x) sum(!is.na(x)))
  if(any(col.count==0)) warning("At least one column is all NAs")
  row.count <- apply(x, 1, function(x) sum(!is.na(x)))
  if(any(row.count==0)) warning("At least one row is all NAs")
  
  # Find a starting column (with fewest number of NAs)
  # startingColumn <- which.max(col.count)
  startingColumn <- ncol(x)
  
  if(verbose >= 1) cat("Starting column: ", startingColumn, "\n")
  
  TotalSS <- sum(x*x, na.rm=TRUE)
  
  eval <- R2cum <- scores <- loadings <- NULL
  anotherPC <- TRUE
  comp <- 1
  
  while(anotherPC) {
    iter <- 0
    t <- x[,startingColumn]
    continue <- TRUE
    if(verbose >= 1) cat(paste("Calculating PC", comp, sep=""))
    
    while(continue) {
      iter <- iter+1
      
      # Calculate LOADINGS p=x't, then normalize
      # Note x*t is column-wise multiplication
      p <- apply(x*t, 2, sum.na)
      p <- p / sqrt(sum(p*p, na.rm=TRUE))
      
      # Calculate SCORES t = xp
      # Cute trick: To get row-wise multiplication, use t(x)*p, then
      # be sure to use apply(,2,) and NOT apply(,1,)!
      t.old <- t
      t <- apply(t(x)*p, 2, sum.na)
      
      # Check convergence criteria
      if (iter > maxiter) stop("Exceeding ", maxiter, " iterations, quitting")
      if( sum((u.old-u)^2, na.rm=TRUE)<tol ) continue=FALSE
      
      if (verbose >= 1) cat(".")
    }
    if (verbose >= 1) cat("\n")
    
    # Remove the estimated principal component from x, x-uv'
    x <- x - (u %*% t(v))
    scores <- cbind(scores, u)
    loadings <- cbind(loadings, v)
    eval <- c(eval, sum(u*u))
    
    # Cumulative proportion of variance
    R2cum <- c(R2cum, 1 - (sum(x*x,na.rm=TRUE) / TotalSS))
    if(comp==maxcomp)
      anotherPC <- FALSE
    else if(R2cum[comp] >= propvar) {
      # Maybe I should set maxcomp=comp here?  Will the user be confused
      # if he requests 10 comps and only 9 are returned?
      maxcomp <- comp
      anotherPC <- FALSE
    } else
      comp <- comp + 1
    
  } # Done finding PCs
  
  # un-cumulate R2
  R2 <- c(R2cum[1], diff(R2cum))
  
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
  out <- list(scores=scores, rotation=as.matrix(loadings),
              completeObs=completeObs,
              maxcomp=maxcomp,
              center=if(is.null(cen)) FALSE else cen,
              scale=if(is.null(sc)) FALSE else sc,
              sdev=apply(scores, 2, sd),
              R2=R2,
              eval=eval,
              propvar=propvar)
  class(out) <- c("nipals","prcomp")
  return(out)
}

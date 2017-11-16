# nipals.R

#' Singular Value Decomposition by non-linear iterative partial least squares
#'
#' Used for finding principal components of a numeric matrix.  
#' Principal Components
#' are extracted one a time.  Missing values in the matrix are allowed.
#' The algorithm computes x = TV', where T is the 'scores' matrix and V is
#' the 'loadings' matrix.
#' 
#' @param x Numerical matrix for which to compute SVD. Missing values allowed.
#' 
#' @param maxcomp Maximum number of principal components to extract.
#'
#' @param maxiter Maximum number of NIPALS iterations to perform for each component.
#'
#' @param tol Default 1e-6 tolerance for testing convergence of the algorithm.
#'
#' @param verbose FALSE. If TRUE, show diagnostic output ('rnipals' only).
#' 
#' @return A list with components \code{eval}, \code{scores}, \code{rotation}, 
#' \code{completeObs}, \code{maxcomp}, 
#' \code{R2}, \code{propvar}.
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
#' p1 <- nipals(B)
#' dim(p1$scores) # 7 x 5
#' dim(p1$rotation) # 5 x 5
#' round(p1$scores %*% diag(p1$eval) %*% t(p1$rotation) - p1$completeObs,2) # 0
#' 
#' B2 = B
#' B2[1,1] = B2[2,2] = NA
#' p2 = nipals(B2)
#' svd(p2$completeObs) # nearly identical to p2
#' 
#' @author Kevin Wright (R version). (With ideas from pcaMethods and 
#' mixOmics packages).
#' 
#' @export
nipals <- function(x, 
                   maxcomp=min(nrow(x), ncol(x)),
                   maxiter=5000,
                   tol=1e-6,
                   verbose=FALSE) {
  
  # A nice summary of NIPALS is here:
  # http://statmaster.sdu.dk/courses/ST02/module06/index.html
  
  # Based partly on mixOmics::nipals
  # https://github.com/cran/mixOmics/blob/master/R/nipals.R
  
  #x <- as.matrix(x)
  nc <- ncol(x)
  nr <- nrow(x)
  x.orig <- x # Save x for replacing missing values

  # Check for a column or row with all NAs
  col.count <- apply(x, 2, function(x) sum(!is.na(x)))
  if(any(col.count==0)) warning("At least one column is all NAs")
  row.count <- apply(x, 1, function(x) sum(!is.na(x)))
  if(any(row.count==0)) warning("At least one row is all NAs")
  
  TotalSS <- sum(x*x, na.rm=TRUE)

  nc.ones = rep(1, nc)
  nr.ones = rep(1, nr)
  
  # initialize outputs
  eval <- rep(NA, length=maxcomp)
  R2cum <- rep(NA, length=maxcomp)
  loadings <- matrix(nrow=nc, ncol=maxcomp)
  scores <- matrix(nrow=nr, ncol=maxcomp)

  for(h in 1:maxcomp) {

    # start column is one with maximum variation
    # this will change as x is deflated
    startcol <- which.max(apply(x, 2, var, na.rm = TRUE))
    if(verbose >= 1) cat("PC ", h, " starting column: ", startcol, sep="")
    u <- x[,startcol]
    u[is.na(u)] = 0

    # replace NA values with 0 so those elements don't contribute
    # to dot-products, etc
    is.na.x <- is.na(x)
    has.na <- any(is.na.x)
    if(has.na){
      x0 <- x
      x0[is.na(x)] <- 0
    }
    
    iter <- 1 # reset for each PC
    continue <- TRUE
    while(continue) {
      
      # calculate LOADINGS v=x'u, then normalize

      if(has.na){
        v = drop(crossprod(x0, u)) # nr x 1 matrix
        # ?? next 3 lines sweep out the eigenvector value, which will
        # give us results exactly like svd(), instead of pca which has 
        # eigenvalues already in the loadings array
        tmp = u %o% nc.ones # matrix with u in each column
        tmp[is.na.x] = 0
        #v = v / diag(crossprod(tmp))
        v = v / colSums(tmp*tmp) # faster
      } else {
        # here it's easier to see eigen value removal
        v = crossprod(x,u) / sum(u*u) # crossprod(u)
      }
      # normalize v to unit-length
      v <- drop(v / sqrt(sum(v*v, na.rm=TRUE)))

      
      # Calculate SCORES u = xv
      u.old <- u
      if (has.na) {
        u = x0 %*% v
        tmp = v %o% nr.ones # v in each column
        tmp[t(is.na.x)] = 0
        u = u / colSums(tmp*tmp)
      } else {
        u = x %*% v / sum(v*v)
      }
      
      # check convergence
      if (iter > maxiter) stop("Exceeding ", maxiter, " iterations, quitting")
      if( sum((u-u.old)^2, na.rm=TRUE)<tol ) continue=FALSE
      
      if (verbose >= 1) cat(".")
      iter <- iter+1
    } # iterations for PC h
    if (verbose >= 1) cat("\n")
    
    # remove variation from x explained by PC, x=x-uv' 
    # same execution time as tcrossprod(u,v) when u,v are vectors
    x <- x - (u %*% t(v)) # 
    loadings[,h] <- v
    scores[,h] <- u
    eval[h] = sum(u*u, na.rm=TRUE)
    
    # Cumulative proportion of variance
    R2cum[h] <- 1 - (sum(x*x,na.rm=TRUE) / TotalSS)
    
  } # Done finding PCs
  
  # un-cumulate R2
  R2 <- c(R2cum[1], diff(R2cum))

  # sweep out eigenvalues from scores
  eval = sqrt(eval)
  scores = sweep(scores, 2, eval, "/")

  # re-construction of x using maxcomp principal components
  fitted.values = scores[ , 1:maxcomp] %*% diag(eval) %*% t(loadings[ , 1:maxcomp])
  
  # replace missing values in the original matrix with fitted values
  completeObs <- x.orig
  completeObs[is.na.x] <- fitted.values[is.na.x]
  
  # output
  rownames(scores) <- rownames(x)
  colnames(scores) <- paste("PC", 1:ncol(scores), sep="")
  rownames(loadings) <- colnames(x)
  colnames(loadings) <- paste("PC", 1:ncol(loadings), sep="")
  
  out <- list(eval=eval,
              scores=scores, 
              rotation=loadings,
              completeObs=completeObs,
              maxcomp=maxcomp,
              R2=R2)
  return(out)
}

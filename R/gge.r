# gge.R
# Time-stamp: <26 Apr 2017 19:00:37 c:/x/rpack/gge/R/gge.R>

if(FALSE) {
  # Tests for 3D
  biplot3d(m2)
  biplot3d(m2, cex.gen=1)
  biplot3d(m2, cex.env=1)
  biplot3d(m2, col.gen="red")
  biplot3d(m2, col.env=c("pink","purple"))
  biplot3d(m2, comps=c(1,2,4))
  biplot3d(m2, lab.env=FALSE)
  biplot3d(m2, res.vec=FALSE)
  biplot3d(m2, zoom.gen=2)
}

# ----------------------------------------------------------------------------

#' Function to create a Red-Gray-Blue palette
#'
#' A function to create a Red-Gray-Blue palette.
#'
#' Using gray instead of white allows missing values to appear as white
#' (actually, transparent).
#'
#' @param n Number of colors to create
#' @return A vector of n colors.
#' @author Kevin Wright
#'
#' @examples
#' pie(rep(1,11), col=RedGrayBlue(11))
#' title("RedGrayBlue(11)")
#' @export
RedGrayBlue <- colorRampPalette(c("firebrick", "lightgray", "#375997"))

# ----------------------------------------------------------------------------

#' GGE biplots
#' 
#' Fit a GGE (genotype + genotype * environment) model and display the results.
#' 
#' If there is replication in G*E, then the replications are averaged together
#' before constructing the biplot.
#' 
#' The singular value decomposition of \code{x} is used to calculate the
#' principal components for the biplot.  Missing values are NOT allowed.
#' 
#' The NIPALS algorithm can be used when there are missing data.
#' 
#' The argument 'method' can be either 'svd' for complete-data, or 'nipals' for
#' missing-data.
#' 
#' @rdname gge
#' @param x A matrix or data.frame.
#' 
#' @param ... Other arguments
#' @return A list of class \code{gge} containing:
#' \item{method}{Method used to calculate principal components.}
#' \item{center}{Data centered?}
#' \item{scale}{Data scaled?}
#' \item{gen.group}{This is only used for plotting.  If not NULL, this specifies a
#' classification of genotypes into groups.  }
#' \item{env.group}{ If not NULL, this specifies a classification of
#' environments into groups.  When using the formula method, then
#' \code{envGroup} is the name of a column in the \code{data}.
#' When using the matrix method, then \code{envGroup} must be a
#' vector the same length as the number of columns in \code{data}, the contents
#' of the vector contain the grouping information.  }
#' @author
#' Jean-Louis Laffont, Kevin Wright
#' 
#' @references
#' 
#' Jean-Louis Laffont, Kevin Wright and Mohamed Hanafi (2013).
#' Genotype + Genotype x Block of Environments (GGB) Biplots.
#' \emph{Crop Science}, 53, 2332-2341.
#' \url{https://doi.org/10.2135/cropsci2013.03.0178}.
#' 
#' Kroonenberg, Pieter M. (1997).
#' \emph{Introduction to Biplots for GxE Tables},
#' Research Report 51, Centre for Statistics, The University of Queensland,
#' Brisbane, Australia.
#' \url{http://three-mode.leidenuniv.nl/document/biplot.pdf}
#' 
#' Yan, W. and Kang, M.S. (2003).
#' \emph{GGE Biplot Analysis}.  CRC Press.
#' 
#' @examples
#' # Example 1.  Data is a data.frame in 'matrix' format
#' B <- matrix(c(50, 67, 90, 98, 120,
#'               55, 71, 93, 102, 129,
#'               65, 76, 95, 105, 134,
#'               50, 80, 102, 130, 138,
#'               60, 82, 97, 135, 151,
#'               65, 89, 106, 137, 153,
#'               75, 95, 117, 133, 155), ncol=5, byrow=TRUE)
#' rownames(B) <- c("G1","G2","G3","G4","G5","G6","G7")
#' colnames(B) <- c("E1","E2","E3","E4","E5")
#' 
#' m1 = gge(B)
#' plot(m1)
#' biplot(m1, title="Example biplot")
#' # biplot3d(m1)
#' 
#' if(require(agridat)){
#'   # crossa.wheat biplot
#' 
#'   # Specify env.group as column in data frame
#'   data(crossa.wheat)
#'   dat2 <- crossa.wheat
#'   dat2$eg <- ifelse(is.element(dat2$loc,
#'                                c("KN","NB","PA","BJ","IL","TC", "JM","PI","AS","ID","SC","SS",
#'                                  "SJ","MS","MG","MM")), "Grp1", "Grp2")
#'   m2 <- gge(yield~gen*loc, dat2, env.group=eg, scale=FALSE)
#'   biplot(m2, lab.env=TRUE, title="crossa.wheat")
#'   # biplot3d(m2)
#' }
#' 
#'
#' @import reshape2
#' @export gge
gge <- function(x, ...) UseMethod("gge")

# ----------------------------------------------------------------------------

#' @param formula A formula
#' 
#' @param data Data frame
#' 
#' @param gen.group genotype group
#' 
#' @param env.group env group
#' 
#' @rdname gge
#' @export
gge.formula <- function(formula, data=NULL,
                        gen.group=NULL, env.group=NULL, ...) {
  # Author: Kevin Wright

  if(is.null(data))
    stop("This usage of gge requires a formula AND data frame.")

  # Get character representations of all necessary variables.
  # There is probably a more R-like (obscure) way to do this, but this works.
  vars <- all.vars(formula)
  # Check for valid names (in the data)
  if(!all(is.element(vars,names(data))))
    stop("Some of the terms in the formula are not found in the data.")
  .y <- vars[1]
  .gen <- vars[2] # Note that 'gen' may already a variable in the data
  .env <- vars[3]

  # Make gen.group & env.group either NULL or quoted name in the data
  gen.group <- substitute(gen.group)
  env.group <- substitute(env.group)
  if(!is.null(gen.group)) {
    gen.group <- deparse(gen.group) # convert to text
    if(!is.element(gen.group, names(data)))
      stop("The argument 'gen.group' refers to non-existant column of data.")

    if(any(colSums(table(data[[gen.group]], data[[.gen]])>0)>1)){
      stop("Some values of '", .gen, "' have multiple gen.group.")
    }
  }
  if(!is.null(env.group)) {
    env.group <- deparse(env.group)
    if(!is.element(env.group, names(data)))
      stop("The argument 'env.group' refers to non-existant column of data.")

    if(any(colSums(table(data[[env.group]], data[[.env]])>0)>1)){
      stop("Some values of '", .env, "' have multiple env.group.")
      env.group <- NULL
    }
  }

  # Finally, reshape data into a matrix, average values in each cell
  # require(reshape2) # Now in 'Depends'
  datm <- reshape2::acast(data, formula(paste(.gen, "~", .env)),
                          fun.aggregate=mean, value.var=.y)
  datm[is.nan(datm)] <- NA # Use NA instead of NaN

  # Make gen.group and env.group to be vectors corresponding to rows/cols of datm
  if(!is.null(gen.group)) {
    ix1 <- match(rownames(datm), data[[.gen]])
    gen.group <- data[[gen.group]][ix1]
  }
  if(!is.null(env.group)) {
    ix2 <- match(colnames(datm), data[[.env]])
    env.group <- data[[env.group]][ix2]
  }

  # Now call the matrix method and return the results
  invisible(gge.matrix(datm, gen.group=gen.group, env.group=env.group, ...))

}

# ----------------------------------------------------------------------------

#' @param center If TRUE, center values for each environment
#' 
#' @param scale If TRUE, scale values for each environment
#' 
#' @param method method used to find principal component directions
#' 
#' @rdname gge
#' @export
gge.matrix <- function(x, center=TRUE, scale=TRUE,
                       gen.group=NULL, env.group = NULL,
                       comps=c(1,2), method="svd", ...) {

  # Author: Kevin Wright, based on S-Plus code by Jean-Louis Laffont.

  # x: matrix of rows=genotypes, cols=environments
  # env.group: vector having the group class for each loc

  if(!is.null(env.group) && (length(env.group) != ncol(x)))
     stop("'env.group' is the wrong length.")
  if(!is.null(gen.group) && (length(gen.group) != nrow(x)))
     stop("'gen.group' is the wrong length.")

  x.orig <- x

  # Check for missing values
  pctMiss <- sum(is.na(x))/(nrow(x)*ncol(x))
  if(pctMiss > 0)
    cat("Missing values detected: (", round(100*pctMiss,0), "%)\n", sep="")
  if(pctMiss > 0 & method=="svd") {
    cat("Switching to 'nipals' method.\n")
    method <- "nipals"
  }
  if(pctMiss > .10)
    warning("Biplots deteriorate for more than 10-15% missing values.")
  genPct <- apply(x, 1, function(xx) length(na.omit(xx)))/ncol(x)
  envPct <- apply(x, 2, function(xx) length(na.omit(xx)))/nrow(x)
  if(any(genPct<.2) || any(envPct<.2))
    warning("Missing data may be structured.")

  # Maximum number of PCs
  maxPCs <- min(nrow(x), ncol(x)-1)

  # Find principal components

  ## # If pcaMethods package is installed, use it
  ## #if(is.element("package:pcaMethods", search())){
  ## if(require("pcaMethods")){

  ##   pcameth <- TRUE

  ##   # cat("Using pcaMethods\n")

  ##   if(!is.element(method, c('bpca', 'nipals', 'ppca', 'rnipals', 'svd', 'svdImpute')))
  ##     stop("Unknown method type for pcaMethods::pca")

  ##   # ----- pcaMethods -----
  ##   # bpca does NOT require orthogonal PCs
  ##   # nipals can handle small amount of missing values
  ##   # ppca allows NA
  ##   # svd is the same as base R svd
  ##   # svdImpute allows NA

  ##   ## if(is.logical(scale) && scale) {
  ##   ##   # warning("Changing scale from TRUE to 'uv' for pcaMethods.\n")
  ##   ##   scale <- "uv"
  ##   ## }
  ##   ## if(is.logical(scale) && !scale) {
  ##   ##   # warning("Changing scale from FALSE to 'none' for pcaMethods.\n")
  ##   ##   scale <- "none"
  ##   ## }

  ##   x.svd <- NULL
  ##   pcascale <- ifelse(scale, 'uv', 'none') # Chg T/F to uv/none
  ##   x.pca <- pca(x, nPcs=min(nrow(x), ncol(x)-1), completeObs=TRUE,
  ##                center=center, scale=pcascale, verbose=TRUE, method=method)
  ##   R2 <- x.pca@R2
  ##   x <- x.pca@completeObs # missing values are replaced with estimates
  ##   x <- scale(x, center=center, scale=scale)

  ## } else { # Use built-in svd or our nipals

  pcameth <- FALSE

  if(!is.element(method, c('nipals', 'svd')))
    stop("Unknown method.  Use 'svd' or 'nipals'")

  # Scale data
  x <- scale(x, center=center, scale=scale)  # Center / scale each environment

  if(method=="svd"){ # ----- SVD -----

    x.pca <- NULL
    x.svd <- svd(x)
    R2 <- x.svd$d^2/sum(x.svd$d^2)

  } else if(method=="nipals"){ # ----- Nipals

    x.svd <- NULL
    x.pca <- nipals(x, completeObs=TRUE, center=FALSE, scale.=FALSE)
    R2 <- x.pca$R2
    x <- x.pca$completeObs # replaces missing values with estimates
    x <- scale(x, center=center, scale=scale)

  }

  ## }

	if(!is.null(x.svd) && length(x.svd$d) == 1)
		stop("Only one principal component.  Biplot not available.")

  # The matrices G,W,R of Laffont et al are here x.g, x.gb, x.r
  # x.g is a matrix of identical columns of genotype means

  genMeans <- rowMeans(x, na.rm=TRUE)
  x.g <-  genMeans %*% t(rep(1, ncol(x)))

  # x.gb is the same size as x, but has the G*B effect
  # First remove gen effects, then average by group, then expand back to size of x
  x.cc <- x - x.g # x.cc = x.orig - envmeans - genmeans
  x.grp <- NULL

  if(is.null(env.group)){
    x.gb <- x.cc # No groups (each loc is its own group)
  } else {
    groupNames <- names(table(env.group))
    for(i in groupNames) {
      # Need 'drop' so that a single-column is not converted to vector
      x.grp <- cbind(x.grp, rowMeans(x.cc[, env.group==i, drop=FALSE]))
    }
    colnames(x.grp) <- groupNames
    x.gb <- x.grp[,match(env.group, colnames(x.grp))]
  }
  # x.r is a matrix of residuals = x.orig - colmeans - rowmeans - G*B
  x.r <- x - x.g - x.gb

  # Orthogonal rotation matrix U
  #if(pcameth) { # pcaMethods package
  #  eval <- apply(x.pca@scores^2, 2, sum) # eigen values
  #  U <- x.pca@scores %*% diag(1/sqrt(eval))
  #} else {
  if(method=="svd") {
    U <- x.svd$u
  } else if (method=="nipals"){
    U <- x.pca$x %*% diag(1/sqrt(x.pca$eval))
  }

  # Partition SSG, SSGB, SSR along each axis
  # Ex: SSGk = diag(u'x.g * x.g'u)
  #          = diag(crossprod(crossprod(x.g, u)))
  #          = colSums(crossprod(x.g, u)^2)
  SSGk <- colSums(crossprod(x.g, U)^2)
  SSGBk <- colSums(crossprod(x.gb, U)^2)
  SSRk <- colSums(crossprod(x.r, U)^2)

  # Data for mosaic plot
  mosdat <- data.frame(G = SSGk, GB = SSGBk, R = SSRk)
  mosdat <- as.matrix(mosdat)
  rownames(mosdat) <- 1:nrow(mosdat)
  names(dimnames(mosdat)) <- c("PC","")

  # Calculate coordinates (along all kept components) for genotypes
  maxcomp <- 5
  maxcomp <- min(maxcomp, nrow(x), ncol(x)-1)
  ROT <- U[ , 1:maxcomp]
  n.gen <- nrow(x)
  genCoord <- ROT * sqrt(n.gen - 1)

  # Block coordinates
  blockCoord <- t(x.g + x.gb) %*% ROT / sqrt(n.gen - 1)

  # Loc coordinates = Block + Residual
  resCoord <- t(x.r) %*% ROT * (1/sqrt(n.gen - 1))
  locCoord <- blockCoord + resCoord
  # locCoord = t(x.g + x.gb + x.r) %*% ROT / sqrt(n.gen -1)
  #  = t(x) %*% ROT / sqrt(n.gen -1)

  # completeObs matrix lacks rownames ?
  rownames(genCoord) <- rownames(x.orig)
  rownames(locCoord) <- colnames(x.orig)
  rownames(blockCoord) <- env.group

  ret <- list(x=x,
              genCoord=genCoord, locCoord=locCoord, blockCoord=blockCoord,
              gen.group=gen.group, env.group=env.group,
              genMeans=genMeans, mosdat=mosdat, R2=R2,
              center=center, scale=scale, method=method,
              pctMiss=pctMiss, maxPCs=maxPCs)
  class(ret) <- "gge"

  return(ret)
}

extend <- function(x,y,xlim,ylim){
  # Given a vector of points (x,y) this function extends the points outward
  # along a vector from (0,0) to the border of the box defined by (xlim,ylim).
  # This box has four 'quadrants' bottom,right,top,left.
  # The 'right' quadrant is a triangle bound by:
  # (0, bottom-right corner, top-right corner)

  xmin <- xlim[1]; xmax <- xlim[2]
  ymin <- ylim[1]; ymax <- ylim[2]

  tr <- atan2(ymax, xmax) # Angle to top-right corner
  tl <- atan2(ymax, xmin) #   top-left
  bl <- atan2(ymin, xmin) #   bottom-left
  br <- atan2(ymin, xmax) #   bottom-right
  phi <- atan2(y, x)      # Angle to each point

  # Instead of many "if-else" terms, just sum(quadrant_indicator * ordinate)
  xb <- (bl < phi & phi <= br) * (ymin*x/y) + # bottom edge
        (br < phi & phi <= tr) * (xmax) +     # right
        (tr < phi & phi <= tl) * (ymax*x/y) + # top
        (phi <= bl | phi > tl) * (xmin)       # left

  yb <- (bl < phi & phi <= br) * (ymin) +
        (br < phi & phi <= tr) * (xmax*y/x) +
        (tr < phi & phi <= tl) * (ymax) +
        (phi <= bl | phi > tl) * (xmin*y/x)

  return(cbind(xb, yb))
}

# ----------------------------------------------------------------------------


#' @rdname gge
#' @export
plot.gge <- function(x, title=substitute(x), ...) {

  # For now, only a mosaic plot.
  # heatmap

  op1 <- par(mfrow=c(2,2))
  R2 <- x$R2

  # Scree plot
  op2 <- par(pty='s', mar=c(3,5,2,1))
  plot(1:length(R2), R2, type="b", axes=FALSE,
       main="", xlab="", ylab="Scree plot - Pct SS")
  axis(1, at=pretty(1:length(R2)), cex.axis=0.75)
  axis(2, at=pretty(c(0,max(R2))), cex.axis=0.75)

  # Mosaic
  par(pty='s', mar=c(2,1,2,1))
  mosaicplot(x$mosdat, main="",
             col=c("darkgreen","lightgreen","gray70"), off=c(0,0))
  mtext(title, line=.5, cex=1)

  # Heatmap
  Y <- x$x
  #par(pty = "m", mar = c(2, 3, 3, 1))
  image(t(Y), col=RedGrayBlue(12), axes=FALSE)
  axis(2, seq(from=0, to=1, length=nrow(Y)), labels=rownames(Y),
       tick=FALSE, cex.axis=.4, col.axis="black", las=2, line=-.8)
  axis(3, seq(from=0, to=1, length=ncol(Y)), labels=colnames(Y),
       tick=FALSE, cex.axis=.4, col.axis="black", las=2, line=-0.8)

  par(op2)
  par(op1)

  invisible()
}

# ----------------------------------------------------------------------------

#' @param title Title, by default the name of the data. Use NULL to suppress the title.
#' 
#' @param subtitle Subtitle to put in front of options. Use NULL to suppress the subtitle.
#'
#' @param xlab Label along axis. Default "auto" shows percent of variation explained. Use NULL to suppress.
#'
#' @param ylab Label along axis. Default "auto" shows percent of variation explained. Use NULL to suppress.
#' 
#' @param cex.gen Character expansion for genotypes
#' 
#' @param cex.env Character expansion for environments
#' 
#' @param col.gen Color for genotypes
#' 
#' @param col.env Color for environments
#' 
#' @param pch.gen Plot character for genotypes
#' 
#' @param lab.env Label environments if TRUE.
#' 
#' @param comps Principal components to use for the biplot. Default c(1,2).
#' 
#' @param flip If "auto" then each axis is flipped so that the genotype
#' ordinate is positively correlated with genotype means.  Can also be a vector
#' like c(TRUE,FALSE) for manual control.
#' 
#' @param origin If "auto", the plotting window is centered on genotypes, otherwise
#' the origin is at the middle of the window.
#' 
#' @param res.vec If TRUE, for each group, draw residual vectors from the mean
#' of the locs to the individual locs.
#' 
#' @param hull If TRUE, show a which-won-where polygon.
#' 
#' @param zoom.gen Zoom factor for manual control of genotype xlim,ylim
#' The default is 1. Values less than 1 may be useful if genotype names are long.
#' 
#' @param zoom.env Zoom factor for manual control of environment xlim,ylim.
#' The default is 1. Values less than 1 may be useful if environment names are long.
#' Not used for 3D biplots.
#' 
#' @rdname gge
#' @import graphics
#' @import grDevices
#' @import stats
#' @export
biplot.gge <- function(x, title = substitute(x), subtitle="",
                       xlab="auto", ylab="auto",
                       cex.gen=0.6, cex.env=.5,
                       col.gen="darkgreen", col.env="orange3",
                       pch.gen=1,
                       lab.env = TRUE,
                       comps=1:2,
                       flip="auto",
                       origin="auto",
                       res.vec=TRUE,
                       hull=FALSE,
                       zoom.gen=1, zoom.env=1,
                       ...){

  # x: A model object of class 'gge'
  # Must include ... because the generic 'biplot' does

  gen.group <- x$gen.group
  env.group <- x$env.group
  genCoord <- x$genCoord
  locCoord <- x$locCoord
  blockCoord <- x$blockCoord
  genMeans <- x$genMeans
  R2 <- x$R2
  pctMiss <- x$pctMiss

  groupNames <- names(table(env.group))
  n.gen.grp <- length(unique(gen.group)) # 0 for NULL
  n.env.grp <- length(unique(env.group))

  # Add options to subtitle
  if(is.null(subtitle)) {
    subtitle = ""
  } else {
    if(subtitle != "") subtitle <- paste0(subtitle, ", ")
    subtitle <- paste0(subtitle, "method=", x$method)
    subtitle <- paste0(subtitle, ", center=", x$center)
    subtitle <- paste0(subtitle, ", scale=", x$scale)
    subtitle <- paste0(subtitle, ", missing: ", round(pctMiss*100,1), "%")
  }

  # Environment (group) colors (first one is used for environments)
  # Replicate colors if not enough have been specified
  col.env <- c(col.env, "blue","black","purple","darkgreen", "red",
               "dark orange", "deep pink", "#999999", "#a6761d")
  if(n.env.grp < 2) {
    col.env <- col.env[1]
  } else {
    col.env <- rep(col.env, length=n.env.grp)
  }

  # Flip. If 'auto', flip the axis so that genotype ordinate is positively
  # correlated with genotype means.
  if(length(flip)<length(comps)) flip <- rep(flip,length=length(comps))
  for(i in 1:length(comps)){
    ix <- comps[i]
    if(flip[i]==TRUE | (flip[i]=="auto" & cor(genMeans, genCoord[,ix]) < 0)){
      locCoord[, ix] <-  - locCoord[, ix]
      genCoord[, ix] <-  - genCoord[, ix]
      blockCoord[, ix] <- - blockCoord[, ix]
    }
  }

  # Initialize plot
  par(pty='s')

  # If alpha transparency is supported, use 70%=180
  if(.Device=="windows" | .Device=="RStudioGD") {
    # These devices do not support true transparency
  } else {
    col.env <- col2rgb(col.env)
    col.env <- rgb(col.env[1,], col.env[2,], col.env[3,],
                   alpha=180, maxColorValue=255)
    col.gen <- col2rgb(col.gen)
    col.gen <- rgb(col.gen[1,], col.gen[2,], col.gen[3,],
                   alpha=180, maxColorValue=255)
  }
  
  xcomp <- comps[1] # Component for x axis
  ycomp <- comps[2] # Component for y axis

  # Axis labels
  labs <- paste("PC ", c(xcomp, ycomp),
                " (", round(100*R2[c(xcomp,ycomp)],0), "% TSS)", sep="")
  if(!is.null(xlab)) {
    if(xlab=="auto") xlab <- labs[1]
  }
  if(!is.null(ylab)) {
    if(ylab=="auto") ylab <- labs[2]
  }

  expand.range <- function(xx) { # Make sure range includes origin
    if(xx[1] > 0) xx[1] <-  - xx[1]
    else if(xx[2] < 0) xx[2] <-  - xx[2]
    return(xx)
  }

  # We are most interested in genotypes, so define the plotting window for
  # genotypes before locations.

  if(origin=="auto"){
  # First, expand the genotype ranges to include 0 in both axes
    rg1 <- expand.range(range(genCoord[, xcomp]))
    rg2 <- expand.range(range(genCoord[, ycomp]))
    # Make genotypes fill as much of the window as possible.  
    xmid <- mean(range(rg1))
    ymid <- mean(range(rg2))
    # Half-width (and half-height) of box. Make axes same length.
    half <- 1.05 * max(diff(rg1), diff(rg2))/2 # Add 5% on each side
  } else {
    xmid <- ymid <- 0
    half <- 1.05 * max(abs(genCoord[, c(xcomp,ycomp)]))
  }
  # Plot limits for genotypes
  xlimg <- c(xmid-half, xmid+half)
  ylimg <- c(ymid-half, ymid+half)

  # Then find the scaling factor to make locations fill the window.
  # Manual tweaking of limits is done with zoom.env
  # Block coord are always 'inside' loc coord box
  re1 <- expand.range(range(locCoord[, xcomp]))
  re2 <- expand.range(range(locCoord[, ycomp]))
  ratio <- max(c(re1, re2)/c(xlimg, ylimg)) * 1.1 # 1.1 adds extra space
  xlime <- xlimg * ratio / zoom.env
  ylime <- ylimg * ratio / zoom.env

  # Set up plot for environment vectors
  plot(NULL, type = "n", xaxt="n", yaxt="n", xlab="", ylab="",
       xlim=xlime, ylim=ylime)

  # Add the margin axis labels and titles
  mtext(xlab, side=1, line=.5, cex=.8)
  mtext(ylab, side=2, line=.5, cex=.8)
  mtext(title, side=3, line=2.5)
  mtext(subtitle, side=3, line=0.9, cex=.7)

  # Note that each environment vector has length 1:
  # round(apply(x$locCoord, 1, function(x) sum(x*x)),2)
  # E1 E2 E3 E4 E5 
  #  1  1  1  1  1
  # For standardized biplots, draw a circle of radius 1 on locCoord scale
  # Do this first so we don't overwrite labels

  if(x$scale) {
    angles <- seq(from=0, to=2*pi, length=100)
    radius <- 1
    xc <- radius * sin(angles)
    yc <- radius * cos(angles)
    lines(xc, yc, col="tan")
  }

  # Plot locs first (points OR labels, but not both) colored by group
  if(is.null(env.group)) {
    eix <- rep(1, nrow(locCoord))
  } else eix <- as.numeric(factor(env.group))

  if(lab.env == TRUE) {
    text(locCoord[ , c(xcomp, ycomp), drop = FALSE],
         rownames(locCoord), cex=cex.env, col = col.env[eix])
  } else {
    points(locCoord[ , c(xcomp, ycomp), drop = FALSE],
           cex = cex.env, col = col.env[eix]) # pch = (1:n.env.grp)[eix])
  }

  # Draw vectors.  Shorten by 5% to reduce over-plotting the label
  if(n.env.grp < 2){
    # Draw vector to each loc
    segments(0, 0, .95*locCoord[,xcomp], .95*locCoord[,ycomp], col = col.env[1])
  } else {
    # Short residual vectors from group mean to each loc
    if(res.vec) {
      segments(blockCoord[ , xcomp], blockCoord[ , ycomp],
               locCoord[ , xcomp], locCoord[ , ycomp],
               col = col.env[eix], lwd = .5)
    }

    # Draw solid-line part of the group vector
    ubc <- blockCoord[groupNames,] # Get unique row for each group
    segments(0, 0, ubc[ , xcomp], ubc[ , ycomp], lwd = 2, col=col.env) # no 'eix'
    # End point
    # points(ubc[ , c(xcomp,ycomp)], pch = 16, col=col.env) # no 'eix'
    # The 'xy' variable extends the vector to the edge of plot
    xy <- extend(ubc[ , xcomp], ubc[ , ycomp], xlime, ylime)
    # Now the extended dashed-line part of the group vector.  Shorten by 10%
    # to reduce over-plotting.
    segments(ubc[ , xcomp], ubc[ , ycomp],
             .90*xy[,1], .90*xy[,2], lty = 3, col=col.env)
    # Add group label
    text(.95*xy[,1], .95*xy[,2], rownames(ubc), cex = 1, col=col.env)
  }

  pch.gen <- c(pch.gen, setdiff(1:20, pch.gen))
  if(n.gen.grp < 2) {
    col.gen <- col.gen[1] # in case there are multiple colors
    pch.gen <- pch.gen[1]
  } else {
    col.gen <- rep(col.gen, length=n.gen.grp)
    pch.gen <- rep(pch.gen, length=n.gen.grp)
  }

  # --- New coordinate system for genotypes ---
  par(new = TRUE)
  # Manual tweaking of limits is done with zoom.gen
  xlimg = xlimg / zoom.gen
  ylimg = ylimg / zoom.gen
  plot(NULL, type = "n", xaxt="n", yaxt="n", xlab="", ylab="",
       xlim=xlimg, ylim=ylimg)
  
  # Now overlay genotype labels and/or points
  if(n.gen.grp < 2) {
    text(genCoord[, c(xcomp, ycomp)], rownames(genCoord), cex=cex.gen, col=col.gen)
  } else {
    gix <- as.numeric(as.factor(gen.group))
    points(genCoord[, c(xcomp, ycomp)], cex=cex.gen, col=col.gen[gix], pch=pch.gen[gix])
    text(genCoord[, c(xcomp, ycomp)], paste(" ",rownames(genCoord)),
         cex=cex.gen, col=col.gen[gix], adj=c(0,.5))
  }

  # Which-won-where polygon
  # http://zonalandeducation.com/mmts/intersections/intersectionOfTwoLines1/intersectionOfTwoLines1.html
  if(hull){
    # Hull polygon
    ch <- chull(genCoord[ , xcomp], genCoord[ , ycomp])
    ch <- c(ch, ch[1])
    segs <- genCoord[ch, c(xcomp, ycomp)]
    lines(segs, col="gray60")
    # Lines perpendicular to hull polygon
    for(ii in 1:(length(ch)-1)){
      x11 <- segs[ii,1] ; y11 <- segs[ii,2]
      x22 <- segs[ii+1,1] ; y22 <- segs[ii+1,2]
      if(x22-x11 == 0) { # Polygon line is vertical
        xnew <- x11
      } else {
        m1 <- (y22-y11) / (x22-x11)        # Slope of polygon line
        m2 <- -1/m1                        # Slope of perp line
        xnew <- (m1 * x11 - y11) / (m1-m2) # Point on polygon line   
      }
      ynew <- m2 * xnew
      # Draw to edge of genotype box
      xy <- extend(xnew, ynew, xlimg, ylimg)
      # Now the extended dashed-line part of the group vector.  Extend 10% to edge.
      segments(0, 0, 1.1*xy[,1], 1.1*xy[,2], lty = 2, col="gray60")
    }
  }
  
  invisible()
}

# ----------------------------------------------------------------------------

#' @rdname gge
#' @export
biplot3d <- function(x, ...) UseMethod("biplot3d", x)

#' @rdname gge
#' @method biplot3d gge
#' @import rgl
#' @export
biplot3d.gge <- function(x,
                         cex.gen=0.6, cex.env=.5,
                         col.gen="darkgreen", col.env="orange3",
                         comps=1:3,
                         lab.env=TRUE,
                         res.vec=TRUE,
                         zoom.gen=1,
                         ...){
  # title/subtitle are not used
  gen.group <- x$gen.group
  env.group <- x$env.group
  genCoord <- x$genCoord
  locCoord <- x$locCoord
  blockCoord <- x$blockCoord
  R2 <- x$R2

  if(is.null(env.group)) n.env.grp <- 0
  else n.env.grp <- length(unique(env.group))

  if(length(R2) == 2)
    stop("Only two principal components--3D biplot not available.")
  if(length(comps) < 3)
    stop("You need to specify 3 components for 3D biplot.")

  # Environment (group) colors (first one is used for environments)
  # Replicate colors if not enough have been specified
  col.env <- c(col.env, "blue","black","purple","darkgreen", "red",
               "dark orange", "deep pink", "#999999", "#a6761d")
  if(n.env.grp < 2) {
    col.env <- col.env[1]
  } else {
    col.env <- rep(col.env, length=n.env.grp)
  }

  # Set up device
  open3d()
  bg3d(color="white")

  xcomp <- comps[1] # Component for x axis
  ycomp <- comps[2] # Component for y axis
  zcomp <- comps[3] # Component for z axis

  # Axis labels
  labs <- paste("PC ", c(xcomp, ycomp, zcomp),
                  " (", round(100*R2[c(xcomp,ycomp)],0), "% TSS)", sep="")
  xlab <- labs[1]
  ylab <- labs[2]
  zlab <- labs[3]

  expand.range <- function(xx) { # Make sure range includes origin
    if(xx[1] > 0) xx[1] <-  - xx[1]
    else if(xx[2] < 0) xx[2] <-  - xx[2]
    return(xx)
  }

  # Determine the range for locs (use same range for all axes)
  envRange <- expand.range(range(locCoord[, c(xcomp,ycomp,zcomp)]))
  xlim <- ylim <- zlim <- range(envRange) * 1.05 # Add 5% for extra space

  # Plot locs first (points OR labels, but not both) colored by group
  if(is.null(env.group)) {
    eix <- rep(1, nrow(locCoord))
  } else eix <- as.numeric(factor(env.group))

  if(lab.env==TRUE){
    text3d(locCoord[,xcomp], locCoord[,ycomp], locCoord[,zcomp],
           texts=rownames(locCoord),
           cex=cex.env, col=col.env[eix], alpha=0.7)
  } else {
    spheres3d(locCoord[,xcomp], locCoord[,ycomp], locCoord[,zcomp],
              radius=0.01*diff(xlim), col=col.env[eix], alpha=0.5)
  }

  # Plot the environments (points OR labels, but not both).
  if(n.env.grp == 0) { # use the same color for all locs
  } else { # color loc by group.
    ##   # Very faint group labels
    ##   text3d(locCoord[,xcomp], locCoord[,ycomp], locCoord[,zcomp],
    ##          texts=rownames(locCoord),
    ##          col=col.env[env.group], alpha=.1)
  }

  # Bounding box
  col.axis <- 'black'

  # Draw axes and label
  lines3d(xlim, rep(ylim[1],2), rep(zlim[1], 2),  col=col.axis, alpha=0.5)
  text3d(xlim[2], ylim[1], zlim[1],
         texts=xlab, cex=cex.env, col=col.axis, alpha=0.5)
  lines3d(rep(xlim[1],2), ylim, rep(zlim[1], 2), col=col.axis, alpha=0.5)
  text3d(xlim[1], ylim[2], zlim[1],
         texts=ylab, cex=cex.env, col=col.axis, alpha=0.5)
  lines3d(rep(xlim[1],2), rep(ylim[1],2), zlim, col=col.axis, alpha=0.5)
  text3d(xlim[1], ylim[1], zlim[2],
         texts=zlab, cex=cex.env, col=col.axis, alpha=0.5)

  # Add vectors (and group labels, if needed)
  if(n.env.grp < 2) {
    # Draw vector to each loc
    apply(cbind(locCoord[,c(xcomp, ycomp, zcomp)], col.env[1]), 1,
          function(xx) {
            segments3d(c(0, xx[1]), c(0, xx[2]), c(0, xx[3]),
                       col=xx[4], alpha=0.5) })
  } else {
    # Short residual vectors from group mean to each loc
    if(res.vec) {
      apply(cbind(blockCoord[ , c(xcomp,ycomp,zcomp)], locCoord[ , c(xcomp,ycomp,zcomp)], col.env[eix]), 1,
            function(xx) {
              segments3d(c(xx[1], xx[4]), c(xx[2], xx[5]), c(xx[3], xx[6]),
                         col = xx[7], alpha=0.5) })
    }

    # Draw solid-line part of the group vector
    apply(cbind(blockCoord[,c(xcomp, ycomp, zcomp)], col.env[eix]), 1,
          function(xx) {
            segments3d(c(0, xx[1]), c(0, xx[2]), c(0, xx[3]),
                       col=xx[4], alpha=0.5, lwd=4) })
    
    # Origin is a black dot
    spheres3d(0,0,0, 
              radius=0.01*diff(xlim),
              col="black", alpha=0.5)
    
    # Add group label
    text3d(blockCoord[, xcomp], blockCoord[, ycomp], blockCoord[, zcomp],
           texts=rownames(blockCoord),
           cex=cex.env*1.5, col=col.env[eix], alpha=0.9)
  }

  # Overlay the genotype names (re-scale to fill the graph)
  genRange <- expand.range(range(genCoord[, c(xcomp,ycomp,zcomp)]))
  ratio <- min(xlim/genRange) * zoom.gen # Why only xlim?
  text3d(genCoord[,xcomp]*ratio, genCoord[,ycomp]*ratio, genCoord[,zcomp]*ratio,
         texts=rownames(genCoord),
         cex=cex.gen, color=col.gen, alpha=0.5)

  return()
}

# ----------------------------------------------------------------------------

nipals <- function(x, maxcomp=min(nrow(x), ncol(x)-1),
                    completeObs=TRUE,
                    maxiter=50*nrow(x),
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

  nr <- nrow(x)
  nc <- ncol(x)

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
  # Choose the column with maximum variation.
  # Maybe we should do this inside the loop for each PC
  #startingColumn <-
  #  which.max(apply(x, 2, function(z) {
  #    z <- na.omit(z)
  #    ifelse(length(z)==1, NA, var(z))
  #  }))
  if(verbose >= 2) cat("Starting column: ", startingColumn, "\n")

  TotalSS <- sum(x*x, na.rm=TRUE)

  eval <- R2cum <- scores <- loadings <- NULL
  anotherPC <- TRUE
  comp <- 1

  while(anotherPC) {
    iter <- 0
    u <- x[,startingColumn]
    continue <- TRUE
    if(verbose >= 1) cat(paste("\nCalculating PC", comp, ": ", sep=""))

    while(continue) {
      iter <- iter+1

      # Calculate LOADINGS v=x'u, then normalize
      # Note x*u is column-wise multiplication
      v <- apply(x*u, 2, sum.na)
      v <- v / sqrt(sum(v*v, na.rm=TRUE))

      # Calculate SCORES u = xv
      # Cute trick: To get row-wise multiplication, use t(x)*v, then
      # be sure to use apply(,2,) and NOT apply(,1,)!
      u.old <- u
      u <- apply(t(x)*v, 2, sum.na)

      # Check convergence criteria
      if (iter > maxiter) stop("Exceeding ", maxiter, " iterations, quitting")
      if( sum((u.old-u)^2, na.rm=TRUE)<tol ) continue=FALSE

      if (verbose >= 1) cat("*")
    }
    if (verbose >= 1) cat(" Done\n")

    # Remove the estimated principal component from x, x-uv'
    x <- x - (u %*% t(v))
    scores <- cbind(scores, u)
    loadings <- cbind(loadings, v)
    eval <- c(eval, sum(u*u))
    if(verbose >= 2) {
      cat("scores\n")
      print(u[1:5,])
      cat("loadings\n")
      print(v[1:5,])
    }

    # Cumulative proportion of variance
    R2cum <- c(R2cum, 1 - (sum(x*x,na.rm=TRUE) / TotalSS))
    if(comp==maxcomp)
      anotherPC <- FALSE
    else if(R2cum[comp] >= propvar) {
      # Maybe I should set maxcomp=comp here?  Will the user be confused
      # if he requests 10 comps and only 9 are returned?
      # maxcomp <- comp
      anotherPC <- FALSE
    } else
      comp <- comp + 1

  } # Done finding PCs

  # Un-cumulate R2
  R2 <- c(R2cum[1], diff(R2cum))

  # This is a re-construction of x using maxcomp principal components
  fitted.values <- scores[ , 1:maxcomp] %*% t(loadings[ , 1:maxcomp])
  if(scale.) fitted.values <- fitted.values * attr(x, "scaled:scale")
  if(center) fitted.values <- fitted.values + attr(x, "scaled:center")

  # Replace missing values in the original matrix with fitted values
  completeObs <- x.orig
  completeObs[is.na(x.orig)] <- fitted.values[is.na(x.orig)]

  # Prepare output
  rownames(scores) <- rownames(x)
  colnames(scores) <- paste("PC", 1:ncol(scores), sep="")
  rownames(loadings) <- colnames(x)
  colnames(loadings) <- paste("PC", 1:ncol(loadings), sep="")
  out <- list(x=scores, rotation=as.matrix(loadings),
              completeObs=completeObs,
              maxcomp=maxcomp,
              center=if(is.null(cen)) FALSE else cen,
              scale=if(is.null(sc)) FALSE else sc,
              sdev=apply(scores, 2, sd),
              R2=R2, nr=nr, nc=nc,
              eval=eval,
              propvar=propvar,
              n.missing=n.missing)
  class(out) <- c("nipals","prcomp")
  return(out)
}

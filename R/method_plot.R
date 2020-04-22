# Based closely on wiqid::diagPlot, now for mcmcOutput

# Function to do multiple trace and density plots
plot.mcmcOutput <- function(x, params, howMany, chains,
  maxRows=4, RhatBad=1.05, precision=c("MCEpc", "n.eff"), ask=NULL, ...) {

  precision <- match.arg(precision)

  # Plotting parameters
  dots <- list(...)
  if(length(dots) == 1 && class(dots[[1]]) == "list")
    dots <- dots[[1]]
  mainTitle <- dots$main # this goes in outer margin
  dots$main <- NULL
  if(is.null(mainTitle))
    mainTitle <- paste("Diagnostics for", deparse(substitute(x)))
  defaultArgs <- list(xlab="", ylab="", type='l', lty=1)
  useArgsT <- useArgsD <- modifyList(defaultArgs, dots)
  selPlot <- names(useArgsT) %in%
    c(names(as.list(args(title))), names(par(no.readonly=TRUE)))
  titleArgs <- mainTitleArgs <- useArgsT[selPlot]
  mainTitleArgs$main <- mainTitle
  mainTitleArgs$line <- dots$line
  mainTitleArgs$adj <- dots$adj
  mainTitleArgs$col.main <- dots$col.main
  mainTitleArgs$outer <- TRUE

  # Conversion to 3d array
  niter <- nrow(x)
  nChains <- attr(x, "nChains")
  draws.per.chain <- niter / nChains
  npars <- ncol(x)
  parnames <-  colnames(x)
  if(is.null(parnames))  # should not happen!
    parnames <- paste0("V", 1:npars)
  mcmc3d <- array(x, dim=c(draws.per.chain, nChains, npars))
  dimnames(mcmc3d) <- list(NULL, 1:nChains, parnames)

  # Deal with subsetting
  if(!missing(params)) {
    params <- matchStart(params, parnames)
    if(length(params) == 0)
      stop("No columns match the specification in 'params'.", call.=FALSE)
    mcmc3d <- mcmc3d[, , params, drop=FALSE]
    parnames <- parnames[params]
    npars <- length(params)
  }
  if(!missing(howMany) && abs(howMany) < draws.per.chain) {  ##### this is wrong
    if(howMany > 0) {
      mcmc3d <- mcmc3d[1:howMany, , , drop=FALSE]
    } else {
      mcmc3d <- mcmc3d[(draws.per.chain+howMany+1):draws.per.chain, , , drop=FALSE]
    }
    draws.per.chain <- dim(mcmc3d)[1]
  }
  if(!missing(chains) && all(chains <= nChains)) {
    mcmc3d <- mcmc3d[, chains, , drop=FALSE]
    nChains <- length(chains)
    niter <- draws.per.chain * nChains
  }

  # Get Rhat and MCEpc or n.eff
  # Rhat : this is same as simplestRhat but with 3d array
  Rhat <- round(simpleRhat3d(mcmc3d), 3)
  # MCEpc
  if(precision=="MCEpc") {
    prec <- round(getMCEpc3d(mcmc3d), 2)
  } else {
    prec <- round(apply(mcmc3d, 3, safeNeff1))
  }

  # Do the plots
  # ------------
  old.par <- par(mar = c(2,2,2,0)+0.1, oma=c(1,1,1,1), "mfrow")
    on.exit(par(old.par))
  if(!is.null(mainTitle))
    par(oma=c(1,1,3,1))

  if(npars > maxRows) {
    if(is.null(ask))
      ask <- dev.interactive(orNone=TRUE)
    old.ask <- devAskNewPage(ask)
    on.exit(devAskNewPage(old.ask), add=TRUE)
  }
  nrows <- min(npars, maxRows)
  layout(matrix(1:(nrows*2), ncol=2, byrow=TRUE), widths=2:1)
  for(i in 1:npars) {
    redFlag <- !is.na(Rhat[i]) && Rhat[i] > RhatBad
    mat <- matrix(mcmc3d[, , i], ncol=nChains) # need 'matrix' if 1 chain
    if(any(is.na(mat))) {
      warning("The chain '", parnames[i], "' contains NAs and cannot be plotted.", call.=FALSE)
      next
    }
    # do trace plot
    useArgsT$ylab <- parnames[i]
    useArgsT$y <- mat
    do.call(matplot, useArgsT)
    abline(h=mean(mat))
    titleArgs$main <- paste0(parnames[i], ": Rhat = ", Rhat[i])
    titleArgs$line <- 0.3
    titleArgs$adj <- 1
    titleArgs$col.main <- 1 + redFlag
    titleArgs$outer <- FALSE
    do.call(title, titleArgs)
    if(redFlag)
      box(col=2, lwd=2)
    # do density plot
    density0(mat, useArgsD)
    # titleArgs$main <- paste0("n.eff = ", n.eff[i])
    titleArgs$main <- paste0(precision, " = ", prec[i])
    titleArgs$adj <- 0
    do.call(title, titleArgs)
    if(redFlag)
      box(col=2, lwd=2)
    do.call(title, mainTitleArgs)
  }
}
# ..........................................................

# Helper function to do density plot (or lollipops) for 1 parameter
# mat : matrix of MCMC output with 1 column per chain
# plotArgs : list with plotting parameters
density0 <- function(mat, plotArgs, ...)  {

  bw <- bw.nrd0(mat)
  # lollipops or density plot?
  # unik <- unique.default(mat)
  # if(length(unik) == 1) {
  if(bw == 0 || diff(range(mat)) < sqrt(.Machine$double.eps)) {
    plot(1, 1, type = "n", ann = FALSE, axes = FALSE)
    text(1,1, paste("All values are the same:\n", signif(mat[1], 4)))
  } else if(all(mat %% 1 == 0) && all(mat >= 0) && diff(range(mat)) < 50) {
    # "lollipops"
    t1 <- apply(mat+1, 2, tabulate, nbins=max(mat)+1)/nrow(mat) # +1 cos tabulate ignores 0
    if(min(mat) > 0)
      t1 <- t1[-(1:min(mat)), ]
    ymax <- apply(t1, 1, max)
    xx <- min(mat):max(mat)
    xlim <- c(min(mat)-0.5, max(mat)+0.5)
    plot(xx, ymax, type='h', col='grey', xlim=xlim)
    abline(h=0)
    abline(v=colMeans(mat), col=1:ncol(mat), lwd=2, lty=3)
    segments(x0 = rep(xx - 0.4, ncol(mat)),
             y0 = t1,
             x1 = rep(xx + 0.4, ncol(mat)),
             y1 = t1,
             col = col(t1))
  } else {
    # density plot
    # deal with folding for probability and non-negative values
    # meanMat <- mean(mat) # do this before folding
    meanMat <- colMeans(mat) # do this before folding
    # use these values if folding is not needed:
    from <- min(mat) - 3*bw
    to <- max(mat) + 3*bw
    mult <- 1
    xx <- mat

    if (min(mat) >= 0 && min(mat) < 2 * bw) {  # it's non-negative
      from <- 0
      xx <- rbind(mat, -mat)
      mult <- 2
    }
    if (min(mat) >= 0 && max(mat) <= 1 &&
          (min(mat) < 2 * bw || 1 - max(mat) < 2 * bw)) { # it's a probability
      to <- min(to, 1)
      xx <- rbind(mat, -mat, 2-mat)
      mult <- 3
    }

    # fit density to each column
    n <- 512
    dens <- apply(xx, 2, function(x) density(x, bw=bw, from=from, to=to, n=n)$y)

    plotArgs$x <- seq(from, to, length.out=n)
    plotArgs$y <- dens * mult
    do.call(matplot, plotArgs)
    abline(h=0, col='grey')
    abline(v=meanMat, col=1:ncol(mat), lwd=2, lty=3)
  }
}
# ..........................................................


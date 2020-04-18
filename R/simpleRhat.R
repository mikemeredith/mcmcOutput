
# simplified Gelman convergence diagnostic using Brooks & Gelman's "interval" method.

# See Brooks & Gelman (1998) General methods for monitoring convergence of iterative simulations. J Computational and Graphical Statistics, 7, 434-455. p. 441

# This follows WinBUGS in using the central 80% interval as the measure of width (WinBUGS manual p.27).

simplestRhat <- function(x) {

  width <- function(y)
    diff(quantile(y, c(0.1, 0.9)))

  nChains <- attr(x, "nChains")

  n.par <- ncol(x)                     # number of parameters
  parNames <- colnames(x)
  if(nChains < 2) {
    Rhat <- rep(NA_real_, n.par)
    names(Rhat) <- parNames
    return(Rhat)
  }

  B <- apply(x, 2, width)              # width of pooled chains

  n.iter <- nrow(x)                    # total sample size
  ipc <- n.iter / nChains              # iterations per chain (T)

  dim(x) <- c(ipc, nChains, n.par)     # separate the chains
  W0 <- apply(x, 2:3, width)           # width of individual chains
  W <- colMeans(W0)

  Rhat <- B / W
  Rhat[is.nan(Rhat)] <- NA
  names(Rhat) <- parNames
  return(Rhat)
}


# An error-catching wrapper for coda::effectiveSize
safeNeff <- function(x) {
  # x is a data frame or matrix with a column for each parameter
  safe1 <- function(v) {
    tmp <- try(coda::effectiveSize(v), silent=TRUE)
    if(inherits(tmp, "try-error"))
      return(NA)
    return(tmp)
  }
  apply(x, 2, safe1)
}


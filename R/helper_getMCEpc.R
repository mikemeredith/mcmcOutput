
# Calculation of MCMC error

# See Lunn et al 2013, The BUGS Book, p77.

# Lunn et al want number of batches = batch size = sqrt(chain length), but only
#   apply that to a single chain; they comment that with multiple chains, number of
#   batches will be larger.
# If there are many chains (eg, 20), this means number of batches >> batch size.
# To achieve number of batches = batch size, we use sqrt(total iterations),
#   corrected to be a multiple of number of chains.

# It gives the same result as coda::batchSE with an appropriate choice of batchSize.

getMCEpc3d <- function(mcmc3d) {

  ipc <- dim(mcmc3d)[1]
  nChains <- dim(mcmc3d)[2]
  npars <- dim(mcmc3d)[3]             # number of parameters
  postSD <- apply(mcmc3d, 3, sd)      # posterior SD

  bpc <- sqrt(ipc * nChains) %/% nChains  # batches per chain (Q)
  bsize <- floor(ipc/bpc)                 # batch size (a)
  ni <- bpc * bsize                       # number of iters per chain to use
  x <- mcmc3d[1:ni, , ]                   # discard unwanted iters
  dim(x) <- c(bsize, bpc, nChains, npars) # separate the batches
  bm <- apply(x, 2:4, mean)               # get batch means
  dim(bm) <- c(bpc*nChains, npars)        # Combine bm's across chains
  sqdev <- (sweep(bm, 2, colMeans(bm), "-"))^2  # Get squared deviation
  SD <- sqrt(colSums(sqdev) * bsize / (nChains*bpc - 1)) # sqrt(rho)
  MCEpc <- SD / sqrt(ipc * nChains) / postSD * 100
  MCEpc[is.nan(MCEpc)] <- NA
  return(MCEpc)
}

getMCEpc <- function(x) {
  return(getMCEpc3d(matTo3d(x)))
}

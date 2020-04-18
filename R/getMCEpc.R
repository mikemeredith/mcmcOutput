
# Calculation of MCMC error

# See Lunn et al 2013, The BUGS Book, p77.

# Lunn et al want number of batches = batch size = sqrt(chain length), but only
#   apply that to a single chain; they comment that with multiple chains, number of
#   batches will be larger.
# If there are many chains (eg, 20), this means number of batches >> batch size.
# To achieve number of batches = batch size, we use sqrt(total iterations),
#   corrected to be a multiple of number of chains.

# It gives the same result as coda::batchSE with an appropriate choice of batchSize.

getMCEpc <- function(x) {

  nChains <- attr(x, "nChains")
  n.par <- ncol(x)                     # number of parameters
  postSD <- apply(x, 2, sd)            # get posterior SD before we mess with the matrix
  parNames <- colnames(x)
  n.iter <- nrow(x)                    # total number of draws
  ipc <- n.iter / nChains              # iterations per chain (T in Lunn et al)

  bpc <- sqrt(n.iter) %/% nChains      # batches per chain (Q)
  bsize <- floor(nrow(x)/nChains/bpc)  # batch size (a)
  ni <- bpc * bsize                    # number of iters per chain to use

  dim(x) <- c(ipc, nChains, n.par)         # separate the chains
  x <- x[1:ni, , ]                         # discard unwanted iters
  dim(x) <- c(bsize, bpc, nChains, n.par)  # separate the batches
  bm <- apply(x, 2:4, mean)                # get batch means
  dim(bm) <- c(bpc*nChains, n.par)         # Combine bm's across chains
  sqdev <- (sweep(bm, 2, colMeans(bm), "-"))^2  # Get squared deviation
  SD <- sqrt(colSums(sqdev) * bsize / (nChains*bpc - 1)) # sqrt(rho)
  MCEpc <- SD / sqrt(n.iter) / postSD * 100
  names(MCEpc) <- parNames
  return(MCEpc)
}

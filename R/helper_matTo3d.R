
# Convert mcmcOutput to a 3d array - not exported

# Retains all the attributes

matTo3d <- function(x) {
  nChains <- attr(x, "nChains")
  npars <- ncol(x)                     # number of parameters
  parnames <- colnames(x)
  ipc <- nrow(x) / nChains             # iterations per chain
  x <- unclass(x)                      # convert to plain old matrix
  dim(x) <- c(ipc, nChains, npars)     # separate the chains
  dimnames(x) <- list(NULL, 1:nChains, parnames)
  return(x)
}


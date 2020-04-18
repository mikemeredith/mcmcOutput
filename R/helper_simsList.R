
# Helper functions for the mcmcOutput generators

mcmcOutput.jagsUI <- function(object, header, ...) {
  name <- deparse(substitute(object))
  mcMat <- mcmcOutput(object$samples)
  # attr(mcMat, "nChains") <- length(object$samples)
  # attr(mcMat, "simsList") <- simsListAttr(mcMat)
  if(missing(header))
    header <- paste("MCMC values from jagsUI object", sQuote(name))
  attr(mcMat, "header") <- header
  return(mcMat)
}
# .......................................................................

# Generate the simsList attribute
simsListAttr <- function(mcMat, ...) {
  # Get parameter names
  params <- colnames(mcMat)
  # Extract base names of parameters
  base <- sapply(strsplit(params, "\\["), "[", 1)
  parNames <- unique(base)

  # Build the list
  simsList <- vector('list', length(parNames))
  names(simsList) <- parNames
  for(i in seq_along(parNames)) {
    simsList[[i]] <- which(base == parNames[i])
    if(length(simsList[[i]]) > 1)   # not scalar, need to convert to array
      #### ISSUE: this will treat p[2,2] as scalar, not a ragged array
      simsList[[i]] <- params2raggedArray(simsList[[i]], params)
  }
  return(simsList)
}
# .......................................................................

# Get an array with indices into a larger matrix
params2raggedArray <- function(index, names) {
  nms <- names[index]
  # Extract the indices from the column names
  t1 <- sapply(strsplit(nms, "\\["), "[", 2)
  indices.c <- unlist(strsplit(t1, "\\]")) # indices as character string

  # Get the max indices = size of output array
  t4 <- simplify2array(strsplit(indices.c, ","))
  if(is.null(dim(t4))) {
    maxind <- max(as.integer(t4))
  } else {
    maxind <- apply(t4, 1, function(x) max(as.integer(x)))
  }

  # Create output array and plug in values
  output <- array(NA, dim=maxind)
  for(i in seq_along(nms)) {
    command <- paste0("output[", indices.c[i], "] <- index[i]")
    eval(parse(text=command))
  }

  # convert 1d array to vector
  if(length(dim(output)) == 1)
    output <- c(output)
  return(output)
}
# ...................................................................


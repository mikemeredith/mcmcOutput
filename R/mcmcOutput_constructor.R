
# Constructor methods to create mcmcArray objects

mcmcOutput <- function(object, ...) UseMethod("mcmcOutput")

mcmcOutput.default <- function(object, ...) {
    stop(paste("No applicable method for class", class(object)))
}
# ...................................................................

mcmcOutput.mcmc.list <- function(object, header, ...) {
  name <- deparse(substitute(object))
  mcMat <- as.matrix(object)
  attr(mcMat, "nChains") <- length(object)
  attr(mcMat, "simsList") <- simsListAttr(mcMat)
  if(missing(header))
    header <- paste("MCMC values from mcmc.list object", sQuote(name))
  attr(mcMat, "header") <- header
  attr(mcMat, "mcpar") <- attr(object[[1]], "mcpar")

  class(mcMat) <- c("mcmcOutput", "matrix", "array")
  return(mcMat)
}
# .......................................................................

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

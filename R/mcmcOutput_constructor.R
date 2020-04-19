
# Constructor methods to create mcmcArray objects

mcmcOutput <- function(object, ...) UseMethod("mcmcOutput")

mcmcOutput.default <- function(object, ...) {
    stop(paste("No applicable method for class", class(object)[1]))
}
# ...................................................................

# Class coda::mcmc.list, used by rjags::coda.samples, jagsUI::jags.basic
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

# Class coda::mcmc for a single chain, rarely used
mcmcOutput.mcmc <- function(object, header, ...) {
  name <- deparse(substitute(object))
  mcMat <- as.matrix(object)
  attr(mcMat, "nChains") <- 1
  attr(mcMat, "simsList") <- simsListAttr(mcMat)
  if(missing(header))
    header <- paste("MCMC values from mcmc object", sQuote(name))
  attr(mcMat, "header") <- header
  attr(mcMat, "mcpar") <- attr(object, "mcpar")

  class(mcMat) <- c("mcmcOutput", "matrix", "array")
  return(mcMat)
}
# .......................................................................

# Class jagsUI::jagsUI used by jagsUI::jags - this class may change in future versions
mcmcOutput.jagsUI <- function(object, header, ...) {
  name <- deparse(substitute(object))
  mcMat <- mcmcOutput(object$samples)
  # attr(mcMat, "nChains") <- length(object$samples)
  # attr(mcMat, "simsList") <- simsListAttr(mcMat)
  if(missing(header))
    header <- paste("MCMC values from jagsUI object", sQuote(name))
  attr(mcMat, "header") <- header
  attr(mcMat, "modelFile") <- object$modfile
  attr(mcMat, "runDate") <- object$run.date
  attr(mcMat, "timeTaken") <- object$mcmc.info$elapsed.mins * 60
  # class(mcMat) <- c("mcmcOutput", "matrix", "array") # not needed
  return(mcMat)
}
# .......................................................................

# Class bugs from R2WinBUGS package and R2OpenBUGS
mcmcOutput.bugs <- function(object, header, ...) {
  name <- deparse(substitute(object))
  # Can't use sims.matrix as the chains are scrambled. Need to start from sims.array
  mcMat <- matrix(object$sims.array, ncol=dim(object$sims.array)[3])
  colnames(mcMat) <- dimnames(object$sims.array)[[3]]
  attr(mcMat, "nChains") <- dim(object$sims.array)[2]
  attr(mcMat, "simsList") <- simsListAttr(mcMat)
  if(missing(header))
    header <- paste("MCMC values from bugs object", sQuote(name))
  attr(mcMat, "header") <- header
  attr(mcMat, "modelFile") <- object$model.file
  class(mcMat) <- c("mcmcOutput", "matrix", "array")
  return(mcMat)
}
# '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

# Class rjags from R2jags package
mcmcOutput.rjags <- function(object, header, ...) {
  name <- deparse(substitute(object))
  mcMat <- object$BUGSoutput$sims.matrix
  attr(mcMat, "nChains") <- object$BUGSoutput$n.chains
  attr(mcMat, "simsList") <- simsListAttr(mcMat)
  if(missing(header))
    header <- paste("MCMC values from rjags object", sQuote(name))
  attr(mcMat, "header") <- header
  attr(mcMat, "modelFile") <- object$model.file
  class(mcMat) <- c("mcmcOutput", "matrix", "array")
  return(mcMat)
}
# '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

# Class runjags from runjags package
mcmcOutput.runjags <- function(object, header, ...) {
  name <- deparse(substitute(object))
  mcMat <- mcmcOutput(object$mcmc)
  # attr(mcMat, "nChains") <- length(object$samples)
  # attr(mcMat, "simsList") <- simsListAttr(mcMat)
  if(missing(header))
    header <- paste("MCMC values from runjags object", sQuote(name))
  attr(mcMat, "header") <- header
  attr(mcMat, "timeTaken") <- as.numeric(object$timetaken, units="secs")
  # class(mcMat) <- c("mcmcOutput", "matrix", "array") # not needed
  return(mcMat)
}
# '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''



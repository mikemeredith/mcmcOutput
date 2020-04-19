
# S3 print method for mcmcOutput objects

print.mcmcOutput <- function(x, ...)  {
  cat("Object of class 'mcmcOutput';")
  cat(" approx. size", format(object.size(x), standard="SI", units="auto", digits=2), "\n")
  header <- attr(x, "header")
  if(!is.null(header))
    cat(header, "\n")
  modelFile <- attr(x, "modelFile")
  if(!is.null(modelFile))
    cat("Model definition file:", modelFile, "\n")
  runDate <- attr(x, "runDate")
  if(!is.null(runDate))
    cat("Model run date:", format(runDate, "%Y-%m-%d %H:%M:%S"), "\n")
  timeTaken <- attr(x, "timeTaken")
  if(!is.null(timeTaken))
    cat("MCMC chain generation took", secs2dhms(timeTaken, 2), "\n")

  nChains <- attr(x, "nChains")
  draws <- nrow(x) / nChains
  cat("The output has", nChains, "chains each with", draws, "draws.\n")
  nPars <- ncol(x)
  elements <- sapply(attr(x, "simsList"), length)
  cat("It has", nPars, "parameters. Top level parameters are:\n")
  print(data.frame(elements))
}
# .........................................................

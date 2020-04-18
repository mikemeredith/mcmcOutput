
# S3 print method for mcmcOutput objects

print.mcmcOutput <- function(x, ...)  {
  cat("Object of class 'mcmcOutput';")
  cat(" approx. size", format(object.size(x), standard="SI", units="auto", digits=2), "\n")
  header <- attr(x, "header")
  if(!is.null(header))
    cat(header, "\n")
  nChains <- attr(x, "nChains")
  draws <- nrow(x) / nChains
  cat("The output has", nChains, "chains each with", draws, "draws.\n")
  nPars <- ncol(x)
  elements <- sapply(attr(x, "simsList"), length)
  cat("It has", nPars, "parameters. Top level parameters are:\n")
  print(data.frame(elements))
}
# .........................................................

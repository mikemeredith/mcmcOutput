
# S3 methods for mcmcOutput objects

# Helper function to build the summary matrix

summarise <- function(x, median=TRUE, CRItype=c("hdi", "symmetrical"), CRImass=0.95,
    Rhat=TRUE, MCEpc = TRUE, ESS=FALSE) {
  
  CRItype <- match.arg(CRItype)
  if(CRItype == "hdi") {
    CRI <- t(apply(x, 2, HDInterval::hdi, credMass=CRImass))
    colnames(CRI) <- c("HDIlo", "HDIup")
  } else {
    tail <- (1 - CRImass)/2
    CRI <- t(apply(x, 2, quantile, probs=c(tail, 1-tail)))
  }
  # nChains <- attr(x, "nChains")
  summary <- cbind(
    "mean" = colMeans(x),
    "sd" = apply(x, 2, sd))
  if(median)
    summary <- cbind(summary, "median" = apply(x, 2, median))
  summary <- cbind(summary, CRI)
  if(Rhat)
    summary <- cbind(summary, "Rhat" = simplestRhat(x))
  if(MCEpc)
    summary <- cbind(summary, "MCS%" = simplestRhat(x))
  
}

summary.mcmcOutput <- function(object, ...)  {
  summary <- attr(object, "summary")
  if(is.null(summary)) {
    summary <- cbind(
      mean = colMeans(object),
      sd = apply(object, 2, sd),
      median = apply(object, 2, median),
      t(apply(object, 2, HDInterval::hdi)))
    colnames(summary)[4:5] <- c("HDIlo", "HDIup")
  }
  return(summary)
}
# .........................................................

print.mcmcOutput <- function(x, digits=3, ...)  {
  toPrint <- attr(x, "summary")
  if(is.null(toPrint)) {
    toPrint <- cbind(
      mean = colMeans(x),
      sd = apply(x, 2, sd),
      median = apply(x, 2, median),
      t(apply(x, 2, HDInterval::hdi)))
    colnames(toPrint)[4:5] <- c("HDIlo", "HDIup")
  }
  print(toPrint, digits = digits)
}
# .........................................................

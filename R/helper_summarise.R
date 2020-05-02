
# S3 methods for mcmcOutput objects

# Helper function to build the summary matrix, not exported

summarise <- function(x, median, CRItype, CRImass, Rhat, MCEpc, n.eff, ...) {

  nChains <- attr(x, "nChains")
  ndraws <- nrow(x)
  draws.per.chain <- ndraws / nChains

  summary <- data.frame(
    "mean" = colMeans(x),
    "sd" = apply(x, 2, sd))
  if(median)
    summary <- cbind(summary, "median" = apply(x, 2, median))
  if(!is.na(CRImass)) {
    if(CRItype == "hdi") {
      CRI <- t(apply(x, 2, HDInterval::hdi, credMass=CRImass))
      colnames(CRI) <- c("HDIlo", "HDIup")
    } else {
      tail <- (1 - CRImass)/2
      CRI <- t(apply(x, 2, quantile, probs=c(tail, 1-tail), na.rm=TRUE))
    }
    summary <- cbind(summary, CRI)
  }
  if(Rhat & nChains > 1 & draws.per.chain > 100)
    summary <- cbind(summary, "Rhat" = simpleRhat(x))
  if(MCEpc & draws.per.chain > 100)
    summary <- cbind(summary, "MCEpc" = getMCEpc(x))
  if(n.eff & ndraws > 100)
    summary <- cbind(summary, "n.eff" = round(safeNeff(x)))
  return(summary)
}


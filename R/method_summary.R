
# S3 methods for mcmcOutput objects

# Helper function to build the summary matrix, not exported

summarise <- function(x, median, CRItype, CRImass, Rhat, MCEpc, n.eff, ...) {

  if(CRItype == "hdi") {
    CRI <- t(apply(x, 2, HDInterval::hdi, credMass=CRImass))
    colnames(CRI) <- c("HDIlo", "HDIup")
  } else {
    tail <- (1 - CRImass)/2
    CRI <- t(apply(x, 2, quantile, probs=c(tail, 1-tail), na.rm=TRUE))
  }
  
  nChains <- attr(x, "nChains")
  ndraws <- nrow(x)
  draws.per.chain <- ndraws / nChains  
  
  summary <- data.frame(
    "mean" = colMeans(x),
    "sd" = apply(x, 2, sd))
  if(median)
    summary <- cbind(summary, "median" = apply(x, 2, median))
  summary <- cbind(summary, CRI)
  if(Rhat & nChains > 1 & draws.per.chain > 100)
    summary <- cbind(summary, "Rhat" = simpleRhat(x))
  if(MCEpc & draws.per.chain > 100)
    summary <- cbind(summary, "MCE%" = getMCEpc(x))
  if(n.eff & ndraws > 100)
    summary <- cbind(summary, "n.eff" = round(safeNeff(x)))
  return(summary)
}

summary.mcmcOutput <- function(object, digits=3, median=TRUE, CRItype=c("hdi", "symmetrical"),
    CRImass=0.95, Rhat=TRUE, MCEpc = TRUE, n.eff=FALSE,...)  {

  CRItype <- match.arg(CRItype)
  sumtab <- summarise(object, median=median, CRItype=CRItype,
    CRImass=CRImass, Rhat=Rhat, MCEpc = MCEpc, n.eff=n.eff, ...)

  header <- attr(object, "header")
  if(!is.null(header))
    cat(header, "\n")
  nChains <- attr(object, "nChains")
  draws <- nrow(object) / nChains
  nPars <- ncol(object)
  cat("The object has", nPars, "parameters with", draws, "draws for each of",
      nChains, "chains.\n")
  if(CRItype == "hdi") {
    cat(paste0("HDIlo and HDIup are the limits of a ", CRImass*100, 
        "% Highest Density Credible Interval.\n"))
  } else {
    lo <- (1 - CRImass)/2 *100
    hi <- 100 - lo
    cat(paste0(lo, "% and ", hi, "% are the limits of a ", CRImass*100, 
        "% Symmetrical Credible Interval.\n"))
  }
  if(Rhat && !is.null(sumtab$Rhat)) {
    cat("Rhat is the estimated potential scale reduction factor:\n")
    R <- sumtab[, 'Rhat']
    t1 <- sum(R > 1.1, na.rm=TRUE)
    t2 <- sum(is.na(R))
    txt <- sprintf("\tlargest is %.2f", max(Rhat, na.rm=TRUE))
    if(t1) {
      txt <- c(txt, sprintf("; %.0f (%.0f%%) are greater than 1.10", t1, 100*t1/length(R)))
    } else {
      txt <- c(txt, "; NONE are greater than 1.10")
    }
    if(t2 > 0)
      txt <- c(txt, sprintf("; %.0f (%.0f%%) are NA", t2, 100*t2/length(R)))
    txt <- c(txt, ".\n")
    cat(paste0(txt, collapse=""))
  }
  if(MCEpc && !is.null(sumtab$"MCE%")) {
    cat("MCE% is the Monte Carlo standard error as a percentage of the posterior SD:\n")
    MCe <- sumtab[, 'MCE%']
    t1 <- sum(MCe > 5, na.rm=TRUE)
    t2 <- sum(is.na(MCe))
    txt <- sprintf("\tlargest is %.1f%%", max(MCe, na.rm=TRUE))
    if(t1) {
      txt <- c(txt, sprintf("; %.0f (%.0f%%) are greater than 5", t1, 100*t1/length(MCe)))
    } else {
      txt <- c(txt, "; NONE are greater than 5%")
    }
    if(t2 > 0)
      txt <- c(txt, sprintf("; %.0f (%.0f%%) are NA", t2, 100*t2/length(MCe)))
    txt <- c(txt, ".\n")
    cat(paste0(txt, collapse=""))
  }
  if(n.eff && !is.null(sumtab$n.eff)) {
    cat("n.eff is the effective number of draws allowing for autocorrelation:\n")
    ne <- sumtab[, 'n.eff']

    ne[ne <= 1] <- NA
    t1 <- sum(ne < 1000, na.rm=TRUE)
    t2 <- sum(is.na(ne))
    txt <- sprintf("\tsmallest is %.0f", min(ne, na.rm=TRUE))
    if(t1) {
      txt <- c(txt, sprintf("; %.0f (%.0f%%) are smaller than 1000", t1, 100*t1/length(ne)))
    } else {
      txt <- c(txt, "; NONE are smaller than 1000")
    }
    if(t2 > 0)
      txt <- c(txt, sprintf("; %.0f (%.0f%%) are 1 or NA", t2, 100*t2/length(ne)))
    txt <- c(txt, ".\n")
    cat(paste0(txt, collapse=""))
  }
  cat("\n")
  return(round(sumtab, digits))
}
# .........................................................


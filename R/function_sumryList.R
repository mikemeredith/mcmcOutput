

# Function to create a summary in list-of-lists format so that foo$mean$theta
#   works and gives output with appropriate structure if theta is an array.

sumryList <- function(object, median=TRUE, CRItype=c("hdi", "symmetrical"),
    CRImass=0.95, Rhat=TRUE, MCEpc = TRUE, n.eff=FALSE,...) {

  if(!inherits(object, "mcmcOutput"))
    stop("The first argument must an object of class 'mcmcOutput'.", call.=FALSE)

  CRItype <- match.arg(CRItype)
  sumtab <- summarise(object, median=median, CRItype=CRItype,
    CRImass=CRImass, Rhat=Rhat, MCEpc = MCEpc, n.eff=n.eff, ...)

  sl <- attr(object, "simsList")
  D <- lapply(sl, dim)
  # function to do 1 column of sumtable
  do1stat <- function(x1) {
    t1 <- lapply(sl, function(x) x1[x])
    for(i in seq_along(t1))
      dim(t1[[i]]) <- D[[i]]
    return(t1)
  }
  return(lapply(sumtab, do1stat))
}

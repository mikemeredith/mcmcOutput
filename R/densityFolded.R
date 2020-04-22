
# Function to do kernel density fitting for a VECTOR
#   with folding for probability and non-negative values.

# This is now used for diagnostic plots with multiple chains in a matrix.

densityFolded <- function(x, bw = "nrd0", adjust = 1, from, to, ...) {
  stopifnot(is.numeric(x))
  nx <- length(x)
  if (is.character(bw)) {  # this code from stats::density
    if (nx < 2)
        stop("need at least 2 points to select a bandwidth automatically")
    bw <- switch(tolower(bw), nrd0 = bw.nrd0(x), nrd = bw.nrd(x),
        ucv = bw.ucv(x), bcv = bw.bcv(x), sj = , `sj-ste` = bw.SJ(x,
            method = "ste"), `sj-dpi` = bw.SJ(x, method = "dpi"),
        stop("unknown bandwidth rule"))
  }
  bw <- bw * adjust

  # use these if folding is not needed:
  if(missing(from))
    from <- min(x) - 3*bw
  if(missing(to))
    to <- max(x) + 3*bw
  xx <- x
  mult <- 1

  # Check for constraints
  if (min(x) >= 0 && min(x) < 2 * bw) { # it's non-negative
    from <- 0
    xx <- c(x, -x)
    mult <- 2
  }
  if (min(x) >= 0 && max(x) <= 1 &&
        (min(x) < 2 * bw || 1 - max(x) < 2 * bw)) { # it's a probability
    xx <- c(x, -x, 2-x)
    mult <- 3
    to <- 1
  }


  dens <- density(xx, bw=bw, adjust=1, from=from, to=to, ...)
  dens$y <- dens$y * mult
  dens$n <- nx
  dens$call <- match.call()
  return(dens)
}

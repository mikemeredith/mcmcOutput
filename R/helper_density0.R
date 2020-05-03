
# Helper function to do density plot (or lollipops) for 1 parameter

# Used by plot.mcmcOutput and densityPlot.

# mat : matrix of MCMC output with 1 column per chain
# plotArgs : list with plotting parameters
density0 <- function(mat, plotArgs, ...)  {

  bw <- bw.nrd0(mat)
  # lollipops or density plot?
  # unik <- unique.default(mat)
  # if(length(unik) == 1) {
  if(bw == 0 || diff(range(mat)) < sqrt(.Machine$double.eps)) {
    plot(1, 1, type = "n", ann = FALSE, axes = FALSE)
    text(1,1, paste("All values are the same:\n", signif(mat[1], 4)))
  } else if(all(mat %% 1 == 0) && all(mat >= 0) && diff(range(mat)) < 50) {
    # "lollipops"
    t1 <- apply(mat+1, 2, tabulate, nbins=max(mat)+1)/nrow(mat) # +1 cos tabulate ignores 0
    if(min(mat) > 0)
      t1 <- t1[-(1:min(mat)), , drop=FALSE]
    ymax <- apply(t1, 1, max)
    xx <- min(mat):max(mat)
    xlim <- c(min(mat)-0.5, max(mat)+0.5)
    plot(xx, ymax, type='h', col='grey', xlim=xlim, xlab="")
    abline(h=0)
    abline(v=colMeans(mat), col=1:ncol(mat), lwd=2, lty=3)
    segments(x0 = rep(xx - 0.4, ncol(mat)),
             y0 = t1,
             x1 = rep(xx + 0.4, ncol(mat)),
             y1 = t1,
             col = col(t1))
  } else {
    # density plot
    # deal with folding for probability and non-negative values
    # meanMat <- mean(mat) # do this before folding
    meanMat <- colMeans(mat) # do this before folding
    # use these values if folding is not needed:
    from <- min(mat) - 3*bw
    to <- max(mat) + 3*bw
    mult <- 1
    xx <- mat

    if (min(mat) >= 0 && min(mat) < 2 * bw) {  # it's non-negative
      from <- 0
      xx <- rbind(mat, -mat)
      mult <- 2
    }
    if (min(mat) >= 0 && max(mat) <= 1 &&
          (min(mat) < 2 * bw || 1 - max(mat) < 2 * bw)) { # it's a probability
      to <- min(to, 1)
      xx <- rbind(mat, -mat, 2-mat)
      mult <- 3
    }

    # fit density to each column
    n <- 512
    dens <- apply(xx, 2, function(x) density(x, bw=bw, from=from, to=to, n=n)$y)

    plotArgs$x <- seq(from, to, length.out=n)
    plotArgs$y <- dens * mult
    do.call(matplot, plotArgs)
    abline(h=0, col='grey')
    abline(v=meanMat, col=1:ncol(mat), lwd=2, lty=3)
  }
}
# ..........................................................


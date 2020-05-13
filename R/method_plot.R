# Function taken from package BEST, original code by John Kruschke.
# Modified by Mike to make best use of ... argument and various other
#  enhancements, including shading of the HDI in showCurve=TRUE plots.


plot.mcmcOutput <- function(x, params, layout=c(3,3), credMass=0.95,
    compVal=NULL, ROPE=NULL, HDItextPlace=0.7, showMode=FALSE,
    showCurve=FALSE, shadeHDI=NULL, ... ) {

  title <- deparse(substitute(x))

  # Deal with ... argument:
  dots <- list(...)
  if(length(dots) == 1 && class(dots[[1]]) == "list")
    dots <- dots[[1]]
  if(!is.null(dots$main)) {
    title <- dots$main
    dots$main <- NULL
  }
  # Deal with subsetting
  if(!missing(params)) {
    params <- matchStart(params, colnames(x))
    if(length(params) == 0)
      stop("No columns match the specification in 'params'.", call.=FALSE)
    x <- x[params]
  }

  nPlots <- ncol(x)

  defaultArgs <- list(
    yaxt="n", ylab="", main="", cex.lab=1.5, cex.main=2,
    cex=1.4, col="skyblue", border="white", bty="n", lwd=5, freq=FALSE)
  useArgs <- modifyList(defaultArgs, dots)
  # Get breaks argument
  breaks <- dots$breaks

  # Deal with layout
  if(nPlots > 1 && nPlots < prod(layout)) {  # adjust the layout
    if(nPlots <= 3) {
      layout <- c(nPlots, 1)
    } else if(nPlots <= 6) {
      layout <- c(ceiling(nPlots/2), 2)
    }
  }

  # Do the plots
  old.par <- par(mar = c(4,1,1,1)+0.1, oma=c(1,1,3,1), "mfrow")
    on.exit(par(old.par))
  if(nPlots > prod(layout)) {
    old.ask <- devAskNewPage(dev.interactive(orNone=TRUE))
    on.exit(devAskNewPage(old.ask), add=TRUE)
  }
  if(nPlots > 1)
    par(mfrow=layout)  # Don't touch mfrow if only 1 plot
  for(i in 1:nPlots) {
    if(is.null(dots$xlab))
      useArgs$xlab <- colnames(x)[i]
    postPlot1(x[, i], credMass=credMass, compVal=compVal, ROPE=ROPE,
        HDItextPlace=HDItextPlace, showMode=showMode, showCurve=showCurve,
             shadeHDI=shadeHDI, useArgs=useArgs, breaks=breaks)
    title(title, outer=TRUE, cex.main=useArgs$cex.main)
  }
}
# .............................................................................


# Does a plot for a single vector (1 parameter)
postPlot1 <- function(x1, credMass, compVal, ROPE, HDItextPlace, showMode, showCurve,
           shadeHDI, useArgs, breaks) {

  # Remove NAs
  x1 <- x1[!is.na(x1)]
  text <- NULL
  if(length(x1) == 0) {
    text <- "No non-NA values."
  } else if(diff(range(x1)) < sqrt(.Machine$double.eps)) {
    text <- paste("All values are the same\n", signifish(x1[1], 4))
  }
  if(!is.null(text)){
    plot(0,0, type = "n", xlim=c(-1,1), ann = FALSE, axes = FALSE)
    title(xlab=useArgs$xlab, cex.lab=useArgs$cex.lab)
    text(0,0, text)
    return(NULL)
  }
  if(is.null(useArgs$xlim))
    useArgs$xlim <- range(compVal, hdi(x1, 0.99))

  if (is.null(breaks)) {
    if (all(x1 == round(x1))) { # all integers
      breaks <- seq(min(x1), max(x1) + 1) - 0.5
    } else {
      nbreaks <- ceiling(diff(range(x1)) /
                          diff(hdi(x1)) * 18)
      breaks <- seq( from=min(x1), to=max(x1),
                     length.out=nbreaks)
    }
  }
  histinfo <- hist(x1, breaks=breaks, plot=FALSE)
  histinfo$xname <- useArgs$xlab

  if (showCurve) {
    densCurve <- densityFolded(x1, adjust=2)
    cenTendHt <- 0.9 * max(densCurve$y)  # For plotting
    selPlot <- names(useArgs) %in%
      c(names(as.list(args(plot.default))), names(par(no.readonly=TRUE)))
    plotArgs <- useArgs[selPlot]
    plotArgs$x <- densCurve$x
    plotArgs$y <- densCurve$y
    plotArgs$type <- "l"
    do.call(plot, plotArgs)
    abline(h=0, col='grey')
    # Display the HDI.
    if(!is.null(credMass)) {
      HDI <- hdi(densCurve, credMass, allowSplit=TRUE)
      ht <- attr(HDI, "height")
      if(nrow(HDI) == 1)  # hdi is not split
        HDI <- matrix(hdi(x1, credMass), nrow=1)
      if(!is.null(shadeHDI))  {
        for (i in 1:nrow(HDI)) {
          inHDI <- which(densCurve$x >= HDI[i, 1] & densCurve$x <= HDI[i, 2])
          polyx <- c(HDI[i, 1], HDI[i, 1], densCurve$x[inHDI], HDI[i, 2], HDI[i, 2])
          polyy <- c(0, ht, densCurve$y[inHDI], ht, 0)
          polygon(polyx, polyy, border=NA, col=shadeHDI)
        }
      } else {
        segments(HDI, 0, HDI, ht, lty=2)
      }
      do.call(lines, plotArgs)
      segments(HDI[, 1], ht, HDI[, 2], ht, lwd=4, lend='butt')
      text( mean(HDI), ht, bquote(.(100*credMass) * "% HDI" ),
            adj=c(.5,-1.7), cex=useArgs$cex, xpd=TRUE )
      # text( HDI, ht, bquote(.(signif(HDI, 3))),
      text( HDI, ht, signifish(HDI, 3),
            pos=3, cex=useArgs$cex, xpd=TRUE )
    }
  } else {
    cenTendHt <- 0.9 * max(histinfo$density)  # For plotting
    plot.histogram.args.names <- c("freq", "density", "angle", "border",
      "main", "sub", "xlab", "ylab", "xlim", "ylim", "axes", "labels",
      "add") # plot.histogram not exported, so need to cheat!
    selPlot <- names(useArgs) %in%
      c(plot.histogram.args.names, names(par(no.readonly=TRUE)))
    plotArgs <- useArgs[selPlot]
    plotArgs$lwd <- 1
    plotArgs$x <- histinfo
    do.call(plot, plotArgs)
    # Display the HDI.
    if(!is.null(credMass)) {
      HDI <- hdi( x1, credMass )
      lines(HDI, c(0,0), lwd=4, lend='butt')
      text( mean(HDI), 0, bquote(.(100*credMass) * "% HDI" ),
            adj=c(.5,-1.7), cex=useArgs$cex, xpd=TRUE )
      text( HDI[1], 0, signifish(HDI[1],3),
            adj=c(HDItextPlace,-0.5), cex=useArgs$cex, xpd=TRUE )
      text( HDI[2], 0, signifish(HDI[2],3),
            adj=c(1.0-HDItextPlace,-0.5), cex=useArgs$cex, xpd=TRUE )
    }
  }

  # Display mean or mode:
  if (showMode==FALSE) {
      meanParam <- mean(x1)
      text(meanParam, cenTendHt,
            bquote(mean==.(signifish(meanParam,3))), adj=c(.5,0), cex=useArgs$cex, xpd=TRUE)
  } else {
      dres <- stats::density(x1)
      modeParam <- dres$x[which.max(dres$y)]
      graphics::text(x=modeParam, y=cenTendHt,
          labels=bquote(mode==.(signifish(modeParam,3))), adj=c(0.5,0), cex=useArgs$cex, xpd=TRUE)
  }
  # Display the comparison value.
  if (!is.null(compVal)) {
    cvHt <- 0.8 * cenTendHt
    cvCol <- "darkgreen"
    pcgtCompVal <- round(100 * mean(x1 > compVal))
    pcltCompVal <- 100 - pcgtCompVal
    lines(c(compVal,compVal), c(0.96*cvHt,0),
            lty="dotted", lwd=1, col=cvCol )
    text(compVal, cvHt,
           bquote(.(pcltCompVal)*"% < " *
                   .(signifish(compVal,3)) * " < "*.(pcgtCompVal)*"%" ),
           adj=c(pcltCompVal/100,0), cex=0.8*useArgs$cex, col=cvCol, xpd=TRUE)
  }
  # Display the ROPE.
  if ( !is.null( ROPE ) ) {
    ROPEtextHt <- 0.55 * cenTendHt
    ropeCol <- "darkred"
    pcInROPE <- mean(x1 > ROPE[1] & x1 < ROPE[2])
    lines(c(ROPE[1],ROPE[1]), c(0.96*ROPEtextHt,0), lty="dotted", lwd=2,
            col=ropeCol)
    lines(c(ROPE[2],ROPE[2]), c(0.96*ROPEtextHt,0), lty="dotted", lwd=2,
            col=ropeCol)
    text(mean(ROPE), ROPEtextHt,
           bquote( .(round(100*pcInROPE))*"% in ROPE" ),
           adj=c(.5,0), cex=1, col=ropeCol, xpd=TRUE )
  }
  # return(invisible(histinfo))
}


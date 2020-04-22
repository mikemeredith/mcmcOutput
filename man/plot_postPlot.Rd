\name{postPlot}
\alias{postPlot}
\title{
Graphic display of a posterior probability distribution
}
\description{
Plot the posterior probability distribution(s) for a vector or array of draws from an MCMC process, with appropriate summary statistics.
}
\usage{
postPlot(object, credMass = 0.95, compVal = NULL, ROPE = NULL,
  HDItextPlace = 0.7, showMode = FALSE, showCurve = FALSE,
  shadeHDI = NULL, layout=c(3,3), ...)
}
\arguments{
  \item{object}{
An object of class \code{mcmcOutput}, or a vector or array of values drawn from the target distribution(s).
}
  \item{credMass}{
the probability mass to include in credible intervals; set this to NULL to suppress plotting of credible intervals.
}
  \item{compVal}{
a value for comparison with those plotted; the same value will be used for all the plots.
}
  \item{ROPE}{
a two element vector, such as \code{c(-1, 1)}, specifying the limits of the Region Of Practical Equivalence; the same value will be used for all the plots.
}
  \item{HDItextPlace}{
a value in [0,1] that controls the horizontal position of the labels at the ends of the HDI bar.
}
  \item{showMode}{
logical: if TRUE, the mode is displayed instead of the mean.
}
  \item{showCurve}{
logical: if TRUE, the posterior density will be represented by a kernel density function instead of a histogram.
}
  \item{shadeHDI}{
specifies a colour to shade the area under the curve corresponding to the HDI; NULL for no shading. Ignored if \code{showCurve = FALSE}. Use \code{colours()} to see a list of possible colours.
}
  \item{layout}{
a length-2 vector with the maximum number of rows and columns to display in the plotting frame.
}
  \item{\dots}{
graphical parameters and the \code{breaks} parameter for the histogram.
}
}
\details{
The data are plotted either as a histogram (above) or, if \code{showCurve = TRUE}, as a fitted kernel density curve (below). Either the mean or the mode of the distribution is displayed, depending on the parameter \code{showMode.} The Highest Density Interval (HDI) is shown as a horizontal bar, with labels for the ends of the interval.

\figure{plotPost1.jpg} \cr
\cr
\figure{plotPost2.jpg}

If values for a ROPE are supplied, these are shown as dark red vertical dashed lines, together with the percentage of probability mass within the ROPE. If a comparison value (\code{compVal}) is supplied, this is shown as a vertical green dotted line, together with the probability mass below and above this value. If values for a ROPE are supplied, these are shown as dark red vertical dashed lines, together with the percentage of probability mass within the ROPE.
}
\value{
Returns nothing. Used for its plotting side-effect.
}
\author{
Mike Meredith, based on code by John Kruschke.
}

\seealso{
For details of the HDI calculation, see \code{\link{hdi}}.
}
\examples{
# Generate some data
normal <- rnorm(1e5, 2, 1)
postPlot(normal)
postPlot(normal, col='wheat', border='magenta')
postPlot(normal, credMass=0.8, compVal=0, ROPE=c(-0.2,0.2), xlab="Response variable")
postPlot(normal, showMode=TRUE, showCurve=TRUE, compVal=5.5)

# For integers:
integers <- rpois(1e5, 12)
postPlot(integers)

# A multi-dimensional array
array <- array(rnorm(4000, 2, 1), dim=c(1000,2,2))
postPlot(array, compVal=0)

# A severely bimodal distribution:
bimodal <- c(rnorm(1e5), rnorm(5e4, 7))
postPlot(bimodal)                  # A valid 95% CrI, but not HDI
postPlot(bimodal, showCurve=TRUE)  # Correct 95% HDI
postPlot(bimodal, showCurve=TRUE, shadeHDI='pink')

}
\keyword{hplot}
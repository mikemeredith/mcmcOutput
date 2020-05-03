\name{mcmcListExample}
\alias{mcmcListExample}
\encoding{UTF-8}
\docType{data}
\title{
An object of class \code{mcmc.list} produced by \code{rjags::coda.samples}
}
\description{
The model has three parameters:
\describe{
  \item{psi }{the probability of occupancy, scalar;}
  \item{p }{probability of detection, a 2 x 2 matrix, but only p[1,1] and p[2,2] are defined in the model, so this is a "ragged array";}
  \item{z }{a vector of length 39, one value for each site.}
}

The number of nodes monitored is 42 (\code{p[1,2]} and \code{p[2,1]} are not monitored).

Three MCMC chains were run, with 1000 adaptation iterations, 1000 burn-in, 10,000 iterations saved after thinning by 10.
}

\usage{data("mcmcListExample")}

\format{
  \code{mcmcListExample} is \code{mcmc.list} object as defined in package \pkg{coda}.
}

\examples{
data(mcmcListExample)
str(mcmcListExample)

# convert to class mcmcOutput
mco <- mcmcOutput(mcmcListExample)
str(mco)

# Extract with "$"
p <- mco$p
str(p)
p[1:5,,]  # Elements of p not defined in the model are filled with NAs

# "[" with one index
head(mco[4:5])
head(mco[c("z[35]", "z[39]")])

# "[" with two indices
mco[1:5, "psi"]  # First 5 values for psi (chain #1)

# "[" with three indices
mco[1:5, 2, "psi"] # First 5 values for psi in chain #2
}
\keyword{datasets}

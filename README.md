mcmcOutput
==========

[![CRAN status](https://www.r-pkg.org/badges/version/mcmcOutput)](https://cran.r-project.org/web/packages/mcmcOutput/index.html)
[![Downloads](https://cranlogs.r-pkg.org/badges/last-month/mcmcOutput)](https://www.r-pkg.org/services)

# An efficient format for storing Monte Carlo chains in R.

R interfaces for JAGS, WinBUGS and OpenBUGS have a variety of formats for storing the MCMC chains produced. Many store more than one copy of the chains in different formats as each has its pros and cons. For example, `R2WinBUGS` output includes a 3-d array, a matrix and a sims.list; `jagsUI`output has two different lists.

The `mcmcOutput` class stores a single copy of the MCMC values, with customised extractor functions which allow it to behave as a data frame, a matrix, a 3-d array, or a sims.list.

A range of constructor methods are provided covering common output formats, and window, print, plot, and summary methods are available.

The `summary` and `diagPlot` functions allow the user to select the diagnostic statistics to display.

The `wiqid` and `saveJAGS` packages now Depend on `mcmcOutput` for the processing of MCMC output.

For more information see [here](https://mmeredith.net/R/mcmcOutput.htm).


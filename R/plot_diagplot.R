# Based closely on wiqid::diagPlot, now for mcmcOutput

# Converts objects to class mcmcOutput and then calls plot.

diagPlot <- function(object, ...) {
  dots <- list(...)
  if(is.null(dots$main)) {
    name <- deparse(substitute(object))
    plot(mcmcOutput(object), main=paste("Diagnostics for", name), ...)
  } else {
    plot(mcmcOutput(object), ...)
  }
}

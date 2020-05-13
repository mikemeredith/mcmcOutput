


postPlot <- function(object, params, layout=c(3,3), credMass=0.95,
    compVal=NULL, ROPE=NULL, HDItextPlace=0.7, showMode=FALSE,
    showCurve=FALSE, shadeHDI=NULL, ... ) {

  dots <- list(...)
  if(is.null(dots$main))
    dots$main <- deparse(substitute(object))

  object <- mcmcOutput(object)
  plot.mcmcOutput(object, params=params, credMass=credMass, compVal=compVal, ROPE=ROPE,
           HDItextPlace=HDItextPlace, showMode=showMode, showCurve=showCurve,
           shadeHDI=shadeHDI, layout=layout, dots)
}

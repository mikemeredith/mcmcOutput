


postPlot <- function(object, credMass=0.95, compVal=NULL, ROPE=NULL,
           HDItextPlace=0.7, showMode=FALSE, showCurve=FALSE,
           shadeHDI=NULL, layout=c(3,3), ... ) {

  dots <- list(...)
  if(is.null(dots$main))
    dots$main <- deparse(substitute(object))
  
  object <- mcmcOutput(object)
  plot.mcmcOutput(object, credMass=credMass, compVal=compVal, ROPE=ROPE,
           HDItextPlace=HDItextPlace, showMode=showMode, showCurve=showCurve,
           shadeHDI=shadeHDI, layout=layout, dots)
}

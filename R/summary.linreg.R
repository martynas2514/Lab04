#' summary of linreg
#'
#' @param obj linreg
#'
#' @return summary
#' @export
summary.linreg <- function(obj){
  
  cat(names(obj$RegressionCoeficients),"\n", obj$RegressionCoeficients)
  cat("\n Residual standard error:", sqrt(obj$ResidualVariance/obj$degreesOfFreedom),"on", obj$degreesOfFreedom,"degrees of freedom")
  cat("\n t - values:", obj$tValues)
  cat("\n residual variance:", obj$ResidualVariance)
  cat("\n degrees of freedom:", obj$degreesOfFreedom)
} 
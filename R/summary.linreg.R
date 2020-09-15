#' summary of linreg
#'
#' @param obj linreg
#'
#' @return summary
#' @export
summary.linreg <- function(obj){
  
  summaryMatrix <- matrix(c(obj$RegressionCoeficients, obj$Pvalues, obj$tValues), ncol = 3)
  rownames(summaryMatrix) <- names(obj$RegressionCoeficients)
  print(summaryMatrix)
  #rownames(summaryMatrix) <- c()
  #cat(names(obj$RegressionCoeficients),"\n", obj$RegressionCoeficients)
  cat("\n Residual standard error:", sqrt(obj$ResidualVariance),"on", obj$degreesOfFreedom,"degrees of freedom")
 # cat("\n t - values:", obj$tValues)
  cat("\n residual variance:", obj$ResidualVariance)
  cat("\n degrees of freedom:", obj$degreesOfFreedom,"\n")
 
} 
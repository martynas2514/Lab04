#' coefficients
#'
#' @param obj linreg object
#'
#' @return coefficients
#' @export
coef.linreg <- function(obj){
  return(obj$RegressionCoeficients)
}
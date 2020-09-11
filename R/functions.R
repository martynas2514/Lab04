#' vector of residuals
#'
#' @param obj linreg object
#'
#' @return vector of residuals
#' @export
residuals.linreg <- function(obj){
  return(as.vector(obj$Residuals))
}

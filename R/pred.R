#' predicted values
#'
#' @param obj 
#'
#' @return predicted values
#' @export
pred <- function(obj){
  UseMethod("pred")
}

#' predicted values
#'
#' @param obj linreg object
#'
#' @return predicted values
#' @export
pred.linreg <- function(obj){
  return(obj$FittedValues)
}
#' @export

print.linreg <- function(obj) {
  cat("linreg (formula = ", format(obj$formula), ", data =", obj$dataName ,")", sep = "")
  cat("\n Coefficients: \n", names(obj$RegressionCoeficients),"\n", obj$RegressionCoeficients)
}
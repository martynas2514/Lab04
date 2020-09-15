#' Multiple regression model
#' 
#' @param formula formula
#' @param data data.frame
#' @return  S3 object of linreg
#' @description  
#' Class takes linear equation and data structure, calculates regression coefficients, fitted values, residuals, degrees of freedom, residual variance, variance of the regression coefficients, t-values and p-values.
#' @export

linreg <- function(formula, data){
  
  stopifnot(inherits(formula(), "formula")  && is.data.frame(data))
  
  X <- model.matrix(formula, data)
  y <- data[all.vars(formula)[1]]
  y <- unname(data.matrix(y))
  regCoef <- solve(t(X) %*% X) %*% t(X) %*% y
  fittedValues <- X %*% regCoef
  residuals <- y - fittedValues
  df <- dim(X)[1] - dim(X)[2]
  residualVariance <- (t(residuals) %*% residuals) / df
  varianceOfRegrCoefficients <- as.vector(residualVariance) * (solve(t(X) %*% X))
  
  tValues <- vector()
  for (i in 1:length(regCoef)) {
    tValues <- append(tValues,regCoef[i]/sqrt(varianceOfRegrCoefficients[i,i]))
  }
  pvalues <- 2*pt(-abs(tValues),df=df)
  values <- list(RegressionCoeficients = as.vector(regCoef),
                 FittedValues = fittedValues,
                 Residuals = residuals,
                 degreesOfFreedom = as.integer(df),
                 ResidualVariance = as.numeric(residualVariance),
                 VarianceOfTheRegressionCoefficients = varianceOfRegrCoefficients,
                 tValues = as.vector(tValues),
                 formula = formula,
                 dataName = deparse(substitute(data)),
                 Pvalues = pvalues
                 )
  names(values$RegressionCoeficients) <- rownames(regCoef)
  class(values) <- "linreg"
  
  
  return(values)
}

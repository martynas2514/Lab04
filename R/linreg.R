#' Multiple regression model
#' 
#' @param formula formula
#' @param data data.frame
#' @return  S3 object of linreg
#' @description  
#' The algorithm takes a graph and an initial node and calculates the shortest path from the initial node to every other node in the graph. Find Wikipedia docs \href{https://cutt.ly/SfE5Er1}{Here}.
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

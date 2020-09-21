#' Multiple regression model
#'
#' @field Formula formula. 
#' @field RegressionCoeficients matrix. 
#' @field FittedValues matrix. 
#' @field Residuals matrix. 
#' @field DegreesOfFreedom numeric. 
#' @field ResidualVariance matrix. 
#' @field VarianceOfTheRegressionCoefficients matrix. 
#' @field TValues vector. 
#' @field DataName character. 
#' @field Pvalues vector. 
#'
#' @return class
#' @import methods
#' @export linreg
#'

#Comment all methods with one line "{text}"

linreg <- setRefClass("linreg",
                       # Include fields -----------
                       fields = list(Formula = "formula",
                                     RegressionCoeficients = "matrix",
                                     FittedValues = "matrix",
                                     Residuals = "matrix",
                                     DegreesOfFreedom = "numeric",
                                     ResidualVariance = "matrix",
                                     VarianceOfTheRegressionCoefficients ="vector",
                                     TValues = "vector",
                                     DataName = "character",
                                     Pvalues = "vector"),
                      
                      # Include methods
                       methods = list(
                         # Initialization of fields 
                         initialize = function(formula, data) { "constructor"
                           Formula <<- formula
                           DataName <<- deparse(substitute(data)) #get the name of dataframe
                           X <- model.matrix(Formula, data)
                           y <- data[all.vars(Formula)[1]]
                           y <- unname(data.matrix(y))
                           
                           # Apply formula to get the regression coefficient matrix
                           RegressionCoeficients <<- solve(t(X) %*% X) %*% t(X) %*% y
                           
                           # Apply formula to get fitted values
                           FittedValues <<- X %*% RegressionCoeficients
                           
                           # Apply formula to get Residuals
                           Residuals <<- y - FittedValues
                           
                           # Apply formula to get the degrees of freedom
                           DegreesOfFreedom <<- dim(X)[1] - dim(RegressionCoeficients)[1]
                           # Number of observations - number of parameters(including intercept)
                           
                           # Apply formula to get the degrees of freedom the Residual Variance 
                           ResidualVariance <<- (t(Residuals) %*% Residuals) / DegreesOfFreedom
                           
                           # Apply formula to get the degrees of freedom the Variance of the Regression coefficient 
                           VarianceOfTheRegressionCoefficients <<- diag(ResidualVariance[1,1] * (solve(t(X) %*% X)))
                           
                           # TValues calculation by means of "for loop" over the Variance Of The Regression Coefficients diagonal
                           
                           TValues <<- as.vector(RegressionCoeficients)/sqrt(VarianceOfTheRegressionCoefficients)
                           
                           Pvalues <<- pt(as.vector(RegressionCoeficients),df=DegreesOfFreedom)
                         },
                         # Print function
                         print = function(){ "prints formula, name of data frame and regression coeficients"

                           cat("Call:\n")
                           cat("linreg(formula = ", format(Formula), ", data = ", DataName ,")\n\n", sep = "")
                           cat("Coefficients:\n")
                           cat(dimnames(RegressionCoeficients)[[1]], "\n")
                           cat(RegressionCoeficients)
                           
                           },
                         # Function that returns Fitted Values
                         pred = function(){ "returns the predicted values"
                           return(FittedValues)
                         },
                         # Function that returns Residuals
                         resid = function(){ "returns vector of residuals"
                           return(as.vector(Residuals))
                         },
                         # Function that returns The Regression Coefficients
                         coef = function(){ "returns coefficients as a named vector"
                           return(RegressionCoeficients)
                         },
                         summary = function(){ "returns the summary of linear regression model"
                           summaryMatrix <- matrix(round(c(as.vector(RegressionCoeficients), as.vector(sqrt(VarianceOfTheRegressionCoefficients)), as.vector(TValues), as.vector(Pvalues)),4), ncol = 4)
                           summaryMatrix <- cbind(summaryMatrix, rep("***",3))
                           colnames(summaryMatrix) <- c("    Coefficients", "Standard Error" ,"Tvalues", "PValues", "***")
                           rownames(summaryMatrix) <- dimnames(RegressionCoeficients)[[1]]
                           cat("Call:\n")
                           cat("linreg(formula = ", format(Formula), ", data = ", DataName ,")\n\n", sep = "")
                           write.table((summaryMatrix), quote = FALSE)
                           cat("\n Residual standard error:", sqrt(ResidualVariance),"on", DegreesOfFreedom,"degrees of freedom")
                         },
                         plot = function() { "plots residuals vs Fitted values plot and Scale - Location plot"
                           
                           tempDataFrame <- data.frame(unlist( Residuals), unlist( FittedValues), c(1:length( Residuals)))
                           names(tempDataFrame) <- c("Residuals", "Fitted_Value", "Index")
                           
                           outliers <- tempDataFrame$Residuals > as.numeric(quantile(tempDataFrame$Residuals)[4]) + (IQR(tempDataFrame$Residuals) * 1.82) |
                           tempDataFrame$Residuals < as.numeric(quantile(tempDataFrame$Residuals)[2]) - (IQR(tempDataFrame$Residuals) * 1.82)
                           
                           
                           A <- ggplot2::ggplot(data = tempDataFrame) +
                             theme_liu()+
                             ggplot2::aes(x = Fitted_Value, y = Residuals) +
                             ggplot2::geom_hline(yintercept = 0, linetype = "dotted") +
                             ggplot2::geom_text(ggplot2::aes(label=ifelse(outliers ,as.character(Index),'')),hjust=1.3,vjust=0) +
                             ggplot2::geom_smooth(ggplot2::aes( x = Fitted_Value, y = Residuals),
                                         formula = y~x,
                                         se = FALSE,
                                         span = 1,
                                         color = "red",
                                         method = "loess") +
                             #ggplot2::geom_line(size = 1, colour = "red") +
                             ggplot2::geom_point(size = 5,
                                                 fill = NA,
                                                 shape = 1) +
                             ggplot2::ggtitle("Residuals vs Fitted") +
                             ggplot2::xlab(paste("Fitted Values", "\n lm(", format( Formula), ")", sep = ""))
                             
                           
                           
                           tempDataFrame <-
                             data.frame(unlist(sqrt(abs( Residuals))), unlist( FittedValues),c(1:length( Residuals)))
                           names(tempDataFrame) <- c("stdResiduals", "Fitted_Value", "Index")
                           
                           B <- ggplot2::ggplot(data = tempDataFrame) +
                             ggplot2::aes(x = Fitted_Value, y = stdResiduals) +
                             ggplot2::geom_text(ggplot2::aes(label=ifelse(outliers ,as.character(Index),'')),hjust=1.3,vjust=0) +
                             #ggplot2::geom_line(size = 1, colour = "red") +
                             ggplot2::geom_point(size = 5,
                                                 fill = NA,
                                                 shape = 1) +
                             ggplot2::geom_smooth(ggplot2::aes( x = Fitted_Value, y = stdResiduals),
                                                  formula = y~x,
                                                  se = FALSE,
                                                  span = 1,
                                                  color = "red",
                                                  method = "loess") +
                             ggplot2::ggtitle("Scale - Location") +
                             ggplot2::xlab(paste("Fitted Values", "\n lm(", format( Formula), ")", sep = "")) +
                             ggplot2::ylab(expression(sqrt(abs("Standardized residuals")))) +
                             theme_liu()
                             
                           
                           gridExtra::grid.arrange(A,B)
                         }
                       )
)

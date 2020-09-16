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
#' @return
#' @import methods
#' @export linreg
#'


linreg <- setRefClass("linreg",
                       fields = list(Formula = "formula",
                                     RegressionCoeficients = "matrix",
                                     FittedValues = "matrix",
                                     Residuals = "matrix",
                                     DegreesOfFreedom = "numeric",
                                     ResidualVariance = "matrix",
                                     VarianceOfTheRegressionCoefficients ="matrix",
                                     TValues = "vector",
                                     DataName = "character",
                                     Pvalues = "vector"),
                       methods = list(
                         initialize = function(formula, data) {
                           Formula <<- formula
                           DataName <<- deparse(substitute(data))
                           X <- model.matrix(Formula, data)
                           y <- data[all.vars(Formula)[1]]
                           y <- unname(data.matrix(y))
                           RegressionCoeficients <<- solve(t(X) %*% X) %*% t(X) %*% y
                           FittedValues <<- X %*% RegressionCoeficients
                           Residuals <<- y - FittedValues
                           DegreesOfFreedom <<- dim(X)[1] - dim(X)[2]
                           ResidualVariance <<- (t(Residuals) %*% Residuals) / DegreesOfFreedom
                           VarianceOfTheRegressionCoefficients <<- as.vector(ResidualVariance) * (solve(t(X) %*% X))
                           
                           TValues <<- vector()
                           for (i in 1:length(RegressionCoeficients)) {
                             TValues <<- append(TValues,RegressionCoeficients[i]/sqrt(VarianceOfTheRegressionCoefficients[i,i]))
                           }
                           Pvalues <<- 2*pt(-abs(TValues),df=DegreesOfFreedom)
                         },
                         print = function(){
                           cat("linreg (formula = ", format(Formula), ", data =", DataName ,")", sep = "")
                           cat("\n Coefficients: \n", names(RegressionCoeficients),"\n", RegressionCoeficients)
                         },
                         pred = function(){
                           return(FittedValues)
                         },
                         resid = function(){
                           return(as.vector(Residuals))
                         },
                         coef = function(){
                           return(RegressionCoeficients)
                         },
                         summary = function(){
                           
                           summaryMatrix <- matrix(c(RegressionCoeficients, Pvalues, TValues), ncol = 3)
                           rownames(summaryMatrix) <- names(RegressionCoeficients)
                           cat(summaryMatrix)
                           #rownames(summaryMatrix) <- c()
                           #cat(names(obj$RegressionCoeficients),"\n", obj$RegressionCoeficients)
                           cat("\n Residual standard error:", sqrt(ResidualVariance),"on", DegreesOfFreedom,"degrees of freedom")
                           # cat("\n t - values:", obj$tValues)
                           cat("\n residual variance:", ResidualVariance)
                           cat("\n degrees of freedom:", DegreesOfFreedom,"\n")
                           
                         },
                         plot = function() {
                           
                           
                           
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
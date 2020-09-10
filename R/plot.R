#' scale - location and Residuals vs Fitted values plots
#'
#' @param obj linreg object
#' @return  plots
#' @description takes linreg object and plots scale - location and Residuals vs
#' Fitted values plots.
#' @export
#' 
plot.linreg <- function(obj) {
  tempDataFrame <- data.frame(obj$Residuals, obj$FittedValues)
  names(tempDataFrame) <- c("Residuals", "Fitted_Value")
  A <- ggplot(data = tempDataFrame) +
    theme(
      plot.title = element_text(
        color = "black",
        size = 14,
        face = "bold",
        hjust = 0.5
      ),
      axis.title.x = element_text(
        color = "black",
        size = 14,
        face = "bold"
      ),
      axis.title.y = element_text(
        color = "black",
        size = 14,
        face = "bold"
      ),
      panel.background = element_rect(fill = "white", colour = "black"),
      axis.text.x = element_text(size = 12, face = "bold"),
      axis.text.y = element_text(size = 12, face = "bold")
    ) +
    aes(x = Fitted_Value, y = Residuals) +
    geom_line(size = 1, colour = "red") +
    geom_point(size = 5,
               fill = NA,
               shape = 1) +
    ggtitle("Residuals vs Fitted") +
    xlab(paste("Fitted Values", "\n lm(", format(obj$formula), ")", sep = "")) +
    
    
    tempDataFrame <-
    data.frame(sqrt(abs(linreg_mod$Residuals)), obj$FittedValues)
  names(tempDataFrame) <- c("stdResiduals", "Fitted_Value")
  
  B <- ggplot(data = tempDataFrame) +
    theme(
      plot.title = element_text(
        color = "black",
        size = 14,
        face = "bold",
        hjust = 0.5
      ),
      axis.title.x = element_text(
        color = "black",
        size = 14,
        face = "bold"
      ),
      axis.title.y = element_text(
        color = "black",
        size = 14,
        face = "bold"
      ),
      panel.background = element_rect(fill = "white", colour = "black"),
      axis.text.x = element_text(size = 12, face = "bold"),
      axis.text.y = element_text(size = 12, face = "bold")
    ) +
    aes(x = Fitted_Value, y = stdResiduals) +
    geom_line(size = 1, colour = "red") +
    geom_point(size = 5,
               fill = NA,
               shape = 1) +
    ggtitle("Scale - Location") +
    xlab(paste("Fitted Values", "\n lm(", format(obj$formula), ")", sep = "")) +
    ylab(expression(sqrt(abs(
      "Standardized residuals"
    ))))
  
  print(A)
  print(B)
  
  
}

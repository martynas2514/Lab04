#' scale - location and Residuals vs Fitted values plots
#'
#' @param obj linreg object
#' @return  plots
#' @description takes linreg object and plots scale - location and Residuals vs
#' Fitted values plots.
#' @export

#require(ggplot2)
plot.linreg <- function(obj) {
  tempDataFrame <- data.frame(unlist(obj$Residuals), unlist(obj$FittedValues))
  names(tempDataFrame) <- c("Residuals", "Fitted_Value")
  A <- ggplot2::ggplot(data = tempDataFrame) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        color = "black",
        size = 14,
        face = "bold",
        hjust = 0.5
      ),
      axis.title.x = ggplot2::element_text(
        color = "black",
        size = 14,
        face = "bold"
      ),
      axis.title.y = ggplot2::element_text(
        color = "black",
        size = 14,
        face = "bold"
      ),
      panel.background = ggplot2::element_rect(fill = "white", colour = "black"),
      axis.text.x = ggplot2::element_text(size = 12, face = "bold"),
      axis.text.y = ggplot2::element_text(size = 12, face = "bold")
    ) +
    ggplot2::aes(x = Fitted_Value, y = Residuals) +
    ggplot2::geom_line(size = 1, colour = "red") +
    ggplot2::geom_point(size = 5,
               fill = NA,
               shape = 1) +
    ggplot2::ggtitle("Residuals vs Fitted") +
    ggplot2::xlab(paste("Fitted Values", "\n lm(", format(obj$formula), ")", sep = ""))
    
    
    tempDataFrame <-
    data.frame(unlist(sqrt(abs(obj$Residuals))), unlist(obj$FittedValues))
  names(tempDataFrame) <- c("stdResiduals", "Fitted_Value")
  
  B <- ggplot2::ggplot(data = tempDataFrame) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        color = "black",
        size = 14,
        face = "bold",
        hjust = 0.5
      ),
      axis.title.x = ggplot2::element_text(
        color = "black",
        size = 14,
        face = "bold"
      ),
      axis.title.y = ggplot2::element_text(
        color = "black",
        size = 14,
        face = "bold"
      ),
      panel.background = ggplot2::element_rect(fill = "white", colour = "black"),
      axis.text.x = ggplot2::element_text(size = 12, face = "bold"),
      axis.text.y = ggplot2::element_text(size = 12, face = "bold")
    ) +
    ggplot2::aes(x = Fitted_Value, y = stdResiduals) +
    ggplot2::geom_line(size = 1, colour = "red") +
    ggplot2::geom_point(size = 5,
               fill = NA,
               shape = 1) +
    ggplot2::ggtitle("Scale - Location") +
    ggplot2::xlab(paste("Fitted Values", "\n lm(", format(obj$formula), ")", sep = "")) +
    ggplot2::ylab(expression(sqrt(abs(
      "Standardized residuals"
    ))))
  
  return(plot(A))
}

#' liu theme
#'
#' @import grDevices
#' @importFrom ggplot2 %+replace%
#' @export



theme_liu <- function(){
  
  #font <- "Miller"   #assign font family up front
  backgroundColor <-  grDevices::rgb(0,185,231, max = 255)
  black <- grDevices::rgb(0,0,0, max = 255)
  white <- grDevices::rgb(255,255,255, max = 255)
  ggplot2::theme_minimal()  %+replace%    #replace elements we want to change
    
    ggplot2::theme(
      
      #grid elements
      #panel.grid.major = element_blank(),    #strip major gridlines
      #panel.grid.minor = element_blank(),    #strip minor gridlines
      #axis.ticks = element_blank(),          #strip axis ticks
      
      #since theme_minimal() already strips axis lines, 
      #we don't need to do that again
      
      panel.grid = ggplot2::element_line(
        colour = white 
      ),

      plot.background = ggplot2::element_rect(
        fill = backgroundColor
      ), 
        
      text = ggplot2::element_text(
        color = white
      ),
      
      panel.border = ggplot2::element_rect(
        fill = NA, 
        colour = white),
      
      axis.text = ggplot2::element_text(
        color = white,
        size = 7
      ),
      axis.title = ggplot2::element_text(
        size = 12
      ),
      plot.title = ggplot2::element_text(
        size = 14,
        hjust = 0.5
      )

    )
}
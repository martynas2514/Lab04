#' liu theme
#'
#' @return
#' @export


theme_liu <- function(){
  library(ggplot2)
  #font <- "Miller"   #assign font family up front
  backgroundColor <-  rgb(0,185,231, max = 255)
  black <- rgb(0,0,0, max = 255)
  white <- rgb(255,255,255, max = 255)
  theme_minimal()  %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      #panel.grid.major = element_blank(),    #strip major gridlines
      #panel.grid.minor = element_blank(),    #strip minor gridlines
      #axis.ticks = element_blank(),          #strip axis ticks
      
      #since theme_minimal() already strips axis lines, 
      #we don't need to do that again
      
      panel.grid = element_line(
        colour = white 
      ),

      plot.background = element_rect(
        fill = backgroundColor
      ), 
        
      text = element_text(
        color = white
      ),
      
      panel.border = element_rect(
        fill = NA, 
        colour = white),
      
      axis.text = element_text(
        color = white,
        size = 7
      ),
      axis.title = element_text(
        size = 12
      ),
      plot.title = element_text(
        size = 14,
        hjust = 0.5
      )

    )
}
copulaTypes <- c("EFGM", "AMH", "Clayton", "Frank", "Gumbel", "Normale")
source("dCopula.R")

shinyServer(function(input, output, session) {
  
  output$copulaPlot <- renderPlotly({
    if (length(input$name) == 0) {
      print("Please select copula type")
    } else {
      
      if (input$name == "EFGM") {
        current_dcopula <- dEFGM
      } else if (input$name == "Clayton") {
        current_dcopula <- dClayton
      } else if(input$name == "AMH") {
        current_dcopula <- dAMH
      } else if(input$name == "Frank") {
        current_dcopula <- dFrank
      } else if(input$name == "Gumbel") {
        current_dcopula <- dGumbel
      } else if(input$name == "Normal") {
        current_dcopula <- dNorm
      }
      
      x.seq <- seq(0, 1, 0.01)
      
      densityMatrix <- outer(x.seq, x.seq, FUN = "current_dcopula", kendallTau = input$kendallTau)
      axz <- list(
        nticks = 10,
        range = c(min(densityMatrix, 0), max(densityMatrix))
      )
      plot_ly(z =~densityMatrix) %>% add_surface() %>% layout(scene = list(zaxis=axz))
      
    }
  })
  
})

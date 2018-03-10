library(shiny)
library(plotly)
library(dplyr)

shinyUI(fluidPage(
  
  titlePanel("Copula density function"),
  
  sidebarPanel(
    h3("Title"),
    selectizeInput("name", label = "Choice of copula",
                   choices = copulaTypes,
                   options = list(maxItems = 1, placeholder = 'Select a copula'),
                   selected = "EFGM")),
  
  sliderParam <- sliderInput('kendallTau', 'Copula parameter', 
                             min = -1, max = 1, value = 0, step = 0.01),
  
  mainPanel(
    textOutput("caption"), 
    plotlyOutput("copulaPlot")
  )
))
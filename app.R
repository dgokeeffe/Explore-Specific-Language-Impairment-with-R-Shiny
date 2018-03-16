library(shiny)
library(tidyverse)
library(shinythemes)
library(plotly)
library(rlang)
source("mood.medtest.R")
source("leveneTest.R")
source("data.R")
source("intro.R")
source("explore.R")
source("graph.R")
#source("predict.R")

ui <- fluidPage(theme = shinytheme("united"),
  tags$style(type="text/css", ".recalculating { opacity: 1.0; }"),
  navbarPage("Diagnose Specific Language Impairment:", 
    tabPanel("Introduction", introUI("intro")),
    tabPanel("Explore", exploreUI("explore")),
    tabPanel("Graph", graphUI("graph")),
    tabPanel("Predict", gapModuleUI("predict"))
  )
)
server <- function(input, output) {
  options(warn = -1)
  callModule(exploreModule, "explore", df)
  callModule(graphModule, "graph", df)
  callModule(predictModule, "predict", all_data)
}

# Run the application 
shinyApp(ui = ui, server = server)
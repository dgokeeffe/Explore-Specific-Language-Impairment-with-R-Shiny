graphUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        # Input: Select a corpus ----
        selectInput(ns("corpus"), "Choose a corpus:",
                    choices = c("All", "ENNI", "Gillam", "Conti-4")),
        
        # Input: Select a group ----
        selectInput(ns("condition"), "Choose a condition:",
                    choices = c("Both", "Specific Language Impaired", "Typically Developing")),
        
        # Input: Specify the feature to view ----
        selectInput(ns("xvar"), "X Feature", axis_vars, selected = "age.x"),
        
        # Input: Specify the feature to view ----
        selectInput(ns("yvar"), "Y Feature", axis_vars, selected = "mlu_words.x"),
        
        # Input: Include X outliers? ----
        radioButtons(ns("outliers_x"), "Include X Outliers?",
                     choices = c("Yes" = 'y',
                                 "No" = "n"),
                     selected = 'y'),
        
        # Input: Include Y outliers? ----
        radioButtons(ns("outliers_y"), "Include Y Outliers?",
                     choices = c("Yes" = 'y',
                                 "No" = "n"),
                     selected = 'y'),
        
        # Input: actionButton() to defer the rendering of output ----
        # until the user explicitly clicks the button (rather than
        # doing it immediately when inputs change). This is useful if
        # the computations required to render output are inordinately
        # time-consuming.
        actionButton(ns("update"), "Update View")
        
      ),
      mainPanel(
        # Output: Header + summary of distribution ----
        h4("Summary"),
        plotlyOutput(ns("scatter"), height = "800px")
      )
    )
  )
}

graphModule <- function(input, output, session, data) {
    
    # Return the requested dataset ----
    # Note that we use eventReactive() here, which depends on
    # input$update (the action button), so that the output is only
    # updated when the user clicks the button
    datasetInput <- eventReactive(input$update, {
        select <- switch(input$corpus,
                         "All" = data,
                         "ENNI" = filter(data, corpus.x == "ENNI"),
                         "Gillam" = filter(data, corpus.x == "Gillam"),
                         "Conti-4" = filter(data, corpus.x == "Conti4")
               )
        select <- switch(input$condition,
                         "Both" = select,
                         "Specific Language Impaired" = filter(select, group.x == "SLI"),
                         "Typically Developing" = filter(select, group.x == "TD")
               )
    }, ignoreNULL = FALSE)
    
    
    
    # Generate boxplots ----
    output$scatter<- renderPlotly({
        # Lables for axes
        xvar_name <- names(axis_vars)[axis_vars == isolate(input$xvar)]
        yvar_name <- names(axis_vars)[axis_vars == isolate(input$yvar)]
        
        dataset <- filter_outliers()
        p <- ggplot(dataset, aes_string(x=isolate(input$xvar), y=isolate(input$yvar), color="group.x")) + 
                    geom_point() +
                    geom_smooth(method=lm) +
                    xlab(xvar_name) +
                    ylab(yvar_name)
        p <- ggplotly(p)
    })
    
    filter_outliers <- function()({
      dataset <- datasetInput()
      if (input$outliers_x == 'n') {
        not_outlier_y <- str_replace(input$yvar, ".x", ".out")
        dataset <- filter_(dataset, paste(not_outlier_y, "== TRUE"))
      }
      if (input$outliers_y == 'n') {
        not_outlier_x <- str_replace(input$yvar, ".x", ".out")
        dataset <- filter_(dataset, paste(not_outlier_x, "== TRUE"))
      }
      return(dataset)
    })
}
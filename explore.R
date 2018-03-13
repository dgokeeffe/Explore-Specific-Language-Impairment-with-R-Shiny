exploreUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        # Input: Select a corpus ----
        selectInput(ns("corpus"), "Choose a corpus:",
                    choices = c("All", "ENNI", "Gillam", "Conti-4")),
        
        # Input: Specify the feature to view ----
        selectInput(ns("fvar"), "Feature", axis_vars, selected = "mlu_words.x"),
        
        # Input: Include outliers? ----
        radioButtons(ns("outliers"), "Include Outliers?",
                     choices = c("Yes" = 'y',
                                 "No" = "n"),
                     selected = 'y'),
        
        # Include clarifying text ----
        helpText("Note: The graphs will only update once the view button is pressed"),
        
        # Input: actionButton() to defer the rendering of output ----
        # until the user explicitly clicks the button (rather than
        # doing it immediately when inputs change). This is useful if
        # the computations required to render output are inordinately
        # time-consuming.
        actionButton(ns("update"), "Update View")
        
      ),
      mainPanel(
        # Output: Header + summary of distribution ----
        h4("Distributions"),
        plotlyOutput(ns("boxplot")),
        br(),
        plotlyOutput(ns("histogram")),
        br(),
        h4("Test of Differences"),
        tableOutput(ns("t_test")),
        helpText("For p-values: Values of 0.000 indicate a p-value less than 3 decimal places large"),
        helpText("For variance tests: The null hypothesis is that the ratio of the variances of the populations from which x and y were drawn, or in the data to which the linear models x and y were fitted, is equal to ratio. In other words, when the p value > 0.05 the variances are equal.")
        
      )
    )
  )
}

exploreModule <- function(input, output, session, data) {
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
    }, ignoreNULL = FALSE)
     
    # Generate boxplots ----
    output$boxplot<- renderPlotly({
        dataset <- filter_outliers()
        
        # Lables for axes
        xvar_name <- names(axis_vars)[axis_vars == isolate(input$fvar)]
        
        # Plot
        p <- ggplot(dataset, aes_string(x="group.x", y=isolate(input$fvar))) + 
                    geom_boxplot() +
                    coord_flip() +
                    # The co-ords are flipped so does the labels
                    ylab(xvar_name) +
                    xlab("Condition")
        p <- ggplotly(p)
    })
  
    # Generate histograms ----
    output$histogram<- renderPlotly({
        dataset <- filter_outliers()
        
        # Lables for axes
        xvar_name <- names(axis_vars)[axis_vars == isolate(input$fvar)]
        
        # Plot
        p <- ggplot(dataset, aes_string(x=isolate(input$fvar), fill = "group.x")) + 
                    geom_histogram(alpha=.5, position="identity") +
                    xlab(xvar_name) +
                    labs(fill='Condition') 
        p <- ggplotly(p)
    })
    
    # Generate t-test ----
    output$t_test <- renderTable({
        dataset <- filter_outliers()
       
        headers <- c("Test, p-value") 
        tests   <- c("Student's t-test", "Mood's Median",
                     "Kruskal-Wallis", "Fligner-Killeen Test of Homogeneity of Variances",
                     "F-Test To Compare Variance")
        results <- c(t.test(get(input$fvar) ~ group.x, data = dataset)$p.value,
                     mood.medtest(get(input$fvar) ~ group.x, data = dataset)$p.value,
                     kruskal.test(get(input$fvar) ~ as.factor(group.x), data = dataset)$p.value,
                     fligner.test(get(input$fvar) ~ as.factor(group.x), data = dataset)$p.value,
                     var.test(get(input$fvar) ~ as.factor(group.x), data = dataset)$p.value)
        df <- data.frame(tests, results)
        names(df)[1] <- "Test"
        names(df)[2] <- "p-value"
        return(df)
    }, digits = 3)
    
    # 
    # # Generate mood's median test ----
    # output$moods-test <- textInput()
    # 
    # # Generate kruskal wallis test ----
    # output$krus <- textInput()
    
    filter_outliers <- function()({
      dataset <- datasetInput()
      if (input$outliers == 'n') {
        not_outlier <- str_replace(input$fvar, ".x", ".out")
        dataset <- filter_(dataset, paste(not_outlier, "== TRUE"))
      }
      return(dataset)
    })
}




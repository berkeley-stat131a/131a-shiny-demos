library(shiny)
library(tidyverse)

ui = fluidPage(
  titlePanel("STAT 131A Law of large numbers and coin flipping simulation"),
  hr(style="border-color: grey;"),
  sidebarLayout(
    sidebarPanel(
      numericInput(
        inputId = "p",
        label = "Probability of heads (p)",
        value = 0.5,
        min = 0,
        max = 1
      ),
      numericInput(
        inputId = "n",
        label = "Number of coins to flip (n)",
        value = 10,
        min = 1,
      ),
      numericInput(
        inputId = "B",
        label = "Number of experiments to run",
        value = 1,
        min = 1,
      ),
      hr(style="border-color: grey;"),
      fluidRow(
        column(width=7,actionButton("play","Run experiments"))
      )
    ),
    
    # plot panel
    mainPanel(
      
      fluidRow(
        column(width=12,textOutput("results"))
      ),
      plotOutput(outputId='hist_results'),
      plotOutput(outputId='boxplot_results'),
     
      
    )
  )
  
)

server = function(input,output){
  
  # reactive to store all reactive variables
  var_list = reactiveValues() 
  
  # current simulation (i.e., sample) number
  var_list$results = c()
  
  forward = function() {
    
    req(input$n)
    req(input$p)
    req(input$B)
      
    samples = rbinom(n=input$B, size=input$n, prob=input$p)
    
    var_list$results = sapply(samples, function(x) x/input$n)
    
  }
  
  # handles the time steps of the animation
  observeEvent(
    eventExpr=input$play,
    handlerExpr={
      forward()
    }
  )
  
  output$results = renderText({
    if (length(var_list$results) > 100) {
      shortened_list = paste0(paste0(var_list$results[1:100], collapse=', '), '...')
    } else {
      shortened_list = paste0(var_list$results, collapse=', ')
    }
    
    paste(length(var_list$results), ' means: [', shortened_list, ']')
  })
  
  # data distribution
  output$hist_results = renderPlot({
    
    if (is.null(var_list$results)) {
      
      # don't render if no data
      return()
      
    } else {
      
      hist(
        var_list$results,
        xlim = c(0, 1),
        main = paste0('Distribution of ', length(var_list$results), ' sample means'),
        xlab = 'Proportion heads'
      )
    
    }
    
  })
  
  output$boxplot_results = renderPlot({
    
    if (is.null(var_list$results)) {
      
      # don't render if no data
      return()
      
    } else {
      
      boxplot(
        var_list$results,
        ylim = c(0, 1),
        xlab = 'Proportion heads',
        horizontal = TRUE
      )
      
    }
    
  })
  
  
}

runApp(shinyApp(ui,server),launch.browser = TRUE)
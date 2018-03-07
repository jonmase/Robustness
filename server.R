# Shiny Server for Robustness Model
# Authors: Jon Mason, Medical Sciences Division Learning Technologies, University of Oxford,
#		   Martin J. Hadley, Academic IT Research Support, University of Oxford
# License: GPL v3, https://www.gnu.org/licenses/gpl.html

library("shiny")
library("sn")

source("visualisation-function.R")

function(input, output, session) {
  #This generates the inputs and allows them to be reset using the reset actionButton
  #See https://stackoverflow.com/questions/24265980/reset-inputs-button-in-shiny-app
  output$resetable_input <- renderUI({
    times <- input$reset
    div(fluidRow(
    column(
      6,
      selectInput(
        "reps",
        h4("Reps"),
        choices = c(100, 500, 1000, 2000, 5000, 10000, 15000, 20000),
        selected = 1000
      )
    ),
    column(6,
           selectInput(
             "sig",
             h4("Sig. Level"),
             choices = c(0.01, 0.05, 0.10),
             selected = 0.05
           )
     )
  ),
  fluidRow(
    column(
      6,
      h4("Distribution 1"),
      selectInput(
        "distribution1",
        h5("Type"),
        choices = c("normal", "skewed"),
        selected = "normal"
      ),
      sliderInput(
        "N1",
        label = h5("N"),
        min = 10,
        max = 200,
        value = 100,
        step = 10
      ),
      sliderInput(
        "mean1",
        label = h5("Mean"),
        min = -2,
        max = 2,
        value = 0,
        step = 0.1
      ),
      sliderInput(
        "sd1",
        label = h5("SD"),
        min = 0.1,
        max = 3,
        value = 1,
        step = 0.1
      ),
      conditionalPanel(
        condition = "input.distribution1 == 'skewed'",
        sliderInput(
          "alpha1",
          label = h5("Alpha (skewness)"),
          min = -20,
          max = 20,
          value = 5
        )
      )
    ),
    column(
      6,
      h4("Distribution 2"),
      selectInput(
        "distribution2",
        h5("Type"),
        choices = c("normal", "skewed"),
        selected = "normal"
      ),
      sliderInput(
        "N2",
        label = h5("N"),
        min = 10,
        max = 200,
        value = 100,
        step = 10
      ),
      sliderInput(
        "mean2",
        label = h5("Mean"),
        min = -2,
        max = 2,
        value = 0,
        step = 0.1
      ),
      sliderInput(
        "sd2",
        label = h5("SD"),
        min = 0.1,
        max = 3,
        value = 1,
        step = 0.1
      ),
      conditionalPanel(
        condition = "input.distribution2 == 'skewed'",
        sliderInput(
          "alpha2",
          label = h5("Alpha (skewness)"),
          min = -20,
          max = 20,
          value = 5
        )
      )
    )
  )
  )
  })
  ##Output the plot
  #req() means that nothing will be shown until these values exist (i.e. the inputs have been rendered)
  output$firstPlot <- renderPlot({
    g(
      distribution1 = req(input$distribution1),
      distribution2 = req(input$distribution2),
      mean1 = req(input$mean1),
      mean2 = req(input$mean2),
      sd1 = req(input$sd1),
      sd2 = req(input$sd2),
      alpha1 = req(input$alpha1),
      alpha2 = req(input$alpha2),
      N1 = req(input$N1),
      N2 = req(input$N2),
      reps = as.numeric(req(input$reps)),
      sig.level = as.numeric(req(input$sig))
    )
    
  }, height = 600)
  
  observeEvent(input$rerun, {
    output$firstPlot <- renderPlot({
      g(
        distribution1 = input$distribution1,
        distribution2 = input$distribution2,
        mean1 = input$mean1,
        mean2 = input$mean2,
        sd1 = input$sd1,
        sd2 = input$sd2,
        alpha1 = input$alpha1,
        alpha2 = input$alpha2,
        N1 = input$N1,
        N2 = input$N2,
        reps = as.numeric(input$reps),
        sig.level = as.numeric(input$sig)
      )
      
    }, height = 600)
  })
}
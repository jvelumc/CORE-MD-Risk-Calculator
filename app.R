# CORE-MD Risk Calculator Shiny app
# Author: Jasper van Egeraat

library(shiny)
library(tidyverse)

ui <- fluidPage(
  
  img(src = "logo.jpg", height="30%", width="30%", align = "right"),
  titlePanel("CORE-MD Calculator"),
  p("Author: Jasper van Egeraat, Bas Penning de Vries, Ewout Steyerberg"),
  p("Version: 4 (2023-10-30)"),
  p("Contact: j.w.a.van_egeraat@lumc.nl"),
  p("This project has received funding from the European Union’s Horizon 2020 research and innovation programme under grant agreement No 965246."),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "in_type", 
        label = "Type of graph",
        choices = list(
          "Sample size plot" = "sample size", 
          "Patient plot" = "patient"),
        selected = "sample size"
      ),
      radioButtons(
        inputId = "in_compare",
        label = "Amount of devices",
        choices = list("Show one device only" = 1, "Compare 2 devices" = 2),
        selected = 1
      ),
      h3("Device 1 properties"),
      textInput(
        inputId = "in_device_1_name",
        label = "Name of device 1",
        value = "device 1"
      ),
      numericInput(
        "in_n_events_1",
        "Observed number of events",
        min = 0,
        value = 0,
        step = 1
      ),
      numericInput(
        "in_empirical_experience_1",
        "Observed cumulative device experience, in person years",
        min = 0,
        value = 500,
        step = 10
      ),
      conditionalPanel(
        condition = "input.in_compare == '2'",
        h3("Device 2 properties"),
        textInput(
          inputId = "in_device_2_name",
          label = "Name of device 2",
          value = "device 2"
        ),
        numericInput(
          "in_n_events_2",
          "Observed number of events",
          min = 0,
          value = 10,
          step = 1
        ),
        numericInput(
          "in_empirical_experience_2",
          "Observed cumulative device experience, in person years",
          min = 0,
          value = 500,
          step = 10
        ),
      ),
      h3("Plot properties"),
      conditionalPanel(
        condition = "input.in_type == 'patient'",
        textInput(
          inputId = "in_title_patient",
          label = "Plot title",
          value = "Device risk for a patient as function of time"
        ),
      ),
      conditionalPanel(
        condition = "input.in_type == 'sample size'",
        textInput(
          inputId = "in_title_sample",
          label = "Plot title",
          value = "Risk as function of total cumulative device experience"
        ),
      ),
      
      numericInput(
        "in_confidence",
        "Confidence",
        min = 1,
        max = 99,
        value = 95,
        step = 1,
      ),
      conditionalPanel(
        condition = "input.in_type == 'patient'",
        numericInput(
          inputId = "in_max_time",
          label = "Time horizon (maximum time in years on x-axis)",
          value = 10,
          min = 1,
          step = 1,
        ),
      ),
      conditionalPanel(
        condition = "input.in_type == 'sample size'",
        numericInput(
          "in_time",
          "Time horizon (time in years at which risk is evaluated)",
          min = 1,
          value = 1,
          step = 1
        ),
        numericInput(
          "in_cumulative_experience",
          "Maximum cumulative device experience, in person-years",
          min = 10,
          value = 1000,
          step = 10
        ),
      ),
      checkboxInput("in_advanced", label = "Show advanced options...", value = FALSE),
      conditionalPanel(
        "input.in_advanced == true",
        checkboxInput("in_checkbox_log", label = "Logarithmic scale", value = TRUE),
        numericInput(
          "in_alpha",
          "Alpha",
          min = 0,
          value = 1,
          step = .001
        ),
        numericInput(
          "in_beta",
          "Beta",
          min = 0,
          value = 0,
          step = .001
        ),
      ),
    ),
    
    mainPanel(
      plotOutput("graph"),
      conditionalPanel(
        condition = "input.in_type == 'sample size'",
        p("The dots mark the upper bound to the n-year risk that we computed for the observed event rate and the observed cumulative device experience. A study with more cumulative device experience, but with the same event rate, will have less uncertainty. Thus, for larger cumulative device experience, we can be confident the n-year risk is lower. This is captured by this graph. " )
      ),
      conditionalPanel(
        condition = "input.in_type == 'patient'",
        textOutput("text")
      )
      
    )
  )
)

server <- function(input, output) {
  output$text <- renderText({paste0("Given the observed data, we can be ", input$in_confidence, "% confident that the risk is below the drawn line. Consider turning off the logarithmic scale in the advanced options.")})
  
  output$graph <- renderPlot({
    
    datapoint <- function(alpha, beta, gamma, time, exp, rate) {
      Risk <- function(lambda, t)
        1 - exp(-lambda * t)
      Qstar <- function(gamma, r, e)
        qgamma(gamma / 100, shape = r * e + alpha, rate = e + beta)
      Q <- function(gamma, r, e, t)
        Risk(Qstar(gamma, r, e), t)
      return(Q(gamma, rate, exp, time))
    }
    
    generate_data <- function(patient_plot) {
      
      alpha <- input$in_alpha
      beta <- input$in_beta
      gamma <- input$in_confidence
      time <- input$in_time
      max_exp <- input$in_cumulative_experience
      
      max_time <- input$in_max_time
      exp1 <- input$in_empirical_experience_1
      exp2 <- input$in_empirical_experience_2
      
      x_axis <- seq(0, ifelse(patient_plot, max_time, max_exp), length = 500)
      
      rate_1 <- input$in_n_events_1 / input$in_empirical_experience_1
      rate_2 <- input$in_n_events_2 / input$in_empirical_experience_2
      
      if (patient_plot) {
        y_values_1 <- datapoint(alpha, beta, gamma, x_axis, exp1, rate_1)
        y_values_2 <- datapoint(alpha, beta, gamma, x_axis, exp2, rate_2)
      }
      else {
        y_values_1 <- datapoint(alpha, beta, gamma, time, x_axis, rate_1)
        y_values_2 <- datapoint(alpha, beta, gamma, time, x_axis, rate_2)
      }
      
      data <- data.frame(x_axis, y_values_1, y_values_2) |> 
        pivot_longer(
          names_to = "device",
          cols = c(y_values_1, y_values_2)
        )
      
      return(data)
    }
    
    
    
    data <- generate_data(input$in_type == "patient")
    
    if (input$in_type == "sample size") {
      xlab <- "Cumulative device experience"
      ylab <- "Risk"
      title <- input$in_title_sample
      caption <- paste0(
        "Graph shows the smallest ", 
        input$in_time, 
        "-year risk that can be excluded with ",
        input$in_confidence,
        "% probability,\ni.e. We can be ",
        input$in_confidence, "% sure the risk is below the drawn line."
      )
    } else {
      xlab <- "Patient years"
      ylab <- "Risk"
      caption <- paste0(
        "Graph shows the smallest risk that can be excluded with",
        input$in_confidence,
        "% probability,\ni.e. We can be ",
        input$in_confidence, "% sure the risk is below the drawn line."
      )
      title <- input$in_title_patient
    }
    
    if (input$in_compare == '1') {
      data <- data |> filter(device == "y_values_1")
    }
    p <- ggplot(data, aes(x = x_axis, y = value, color = device)) + 
      geom_line() + 
      scale_color_hue(labels = c(input$in_device_1_name, input$in_device_2_name)) +
      xlab(xlab) + 
      ylab(ylab) + 
      labs(title = title, caption = caption) + 
      theme(text=element_text(size=14), #change font size of all text
            axis.title=element_text(size=12), #change font size of axis titles
            plot.title=element_text(size=16), #change font size of plot title
            legend.text=element_text(size=14), #change font size of legend text
            legend.title=element_text(size=14)) #change font size of legend title   
    
    if (input$in_type == "sample size") {
      p <- p + annotate("point", x = input$in_empirical_experience_1, y = datapoint(
        alpha = input$in_alpha,
        beta = input$in_beta,
        gamma = input$in_confidence,
        time = input$in_time,
        exp = input$in_empirical_experience_1,
        rate = input$in_n_events_1/input$in_empirical_experience_1
      ), color = "red") 
      if (input$in_compare == "2")
        p <- p + annotate("point", x = input$in_empirical_experience_2, y = datapoint(
          alpha = input$in_alpha,
          beta = input$in_beta,
          gamma = input$in_confidence,
          time = input$in_time,
          exp = input$in_empirical_experience_2,
          rate = input$in_n_events_2/input$in_empirical_experience_2
        ), color = "blue") 
    }
    
    if (input$in_checkbox_log) {
      p <- p + scale_y_continuous(trans = 'log10')
    }
    
    return(p)
  }, width = 700)
}

# Run the application 
shinyApp(ui = ui, server = server)

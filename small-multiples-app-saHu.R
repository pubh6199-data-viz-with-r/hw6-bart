library(shiny)
library(ggplot2)
library(dplyr)
library(bslib)
library(patchwork)
library(lubridate)

# Create sample time series data for 4 individuals
create_sample_timeseries <- function() {
  # Create dates
  dates <- seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day")
  
  # Create 4 individuals with different patterns
  set.seed(123)
  
  # Function to create a time series with some randomness
  create_ts <- function(base, amplitude, freq, noise) {
    base + amplitude * sin(freq * 1:length(dates)) + rnorm(length(dates), 0, noise)
  }
  
  # Create data for each individual
  individual1 <- create_ts(25, 5, 0.05, 2)
  individual2 <- create_ts(22, 3, 0.03, 1.5)
  individual3 <- create_ts(28, 4, 0.02, 2.5)
  individual4 <- create_ts(20, 6, 0.04, 1.8)
  
  # Combine into a data frame
  data.frame(
    date = rep(dates, 4),
    value = c(individual1, individual2, individual3, individual4),
    individual = factor(rep(c("Person A", "Person B", "Person C", "Person D"), each = length(dates)))
  )
}

# UI
ui <- page_sidebar(
  title = "Small Multiples Visualization",
  sidebar = sidebar(
    title = "Controls",
    selectInput("dataset", "Choose a dataset:",
                choices = c("Time Series", "mtcars", "iris", "diamonds")),
    uiOutput("x_var"),
    uiOutput("y_var"),
    uiOutput("facet_var"),
    conditionalPanel(
      condition = "input.dataset == 'Time Series'",
      checkboxInput("show_average", "Show group average", value = TRUE),
      selectInput("line_type", "Plot type:", 
                  choices = c("Line" = "line", "Line + Points" = "line_point"), 
                  selected = "line")
    ),
    sliderInput("ncol", "Number of columns:", min = 1, max = 5, value = 2)
  ),
  card(
    card_header("Small Multiples Visualization"),
    card_body(
      plotOutput("smallMultiples", height = "500px")
    )
  ),
  card(
    card_header("What are Small Multiples?"),
    card_body(
      p("Small multiples (also called trellis charts or facet plots) are a series of similar graphs or charts using the same scale and axes, displayed together for easy comparison."),
      p("This visualization technique helps to compare patterns across different categories or time periods, making it easier to identify trends, outliers, and relationships."),
      p("In this example, we've added a time series dataset with 4 individuals, with an option to display the group average as a reference line in each facet.")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive value for the selected dataset
  selected_data <- reactive({
    switch(input$dataset,
           "Time Series" = create_sample_timeseries(),
           "mtcars" = mtcars %>% mutate(car = rownames(mtcars), cyl = as.factor(cyl), am = as.factor(am), vs = as.factor(vs), gear = as.factor(gear), carb = as.factor(carb)),
           "iris" = iris,
           "diamonds" = diamonds %>% sample_n(1000))  # Sample for better performance
  })
  
  # Dynamic UI for variable selection based on the dataset
  output$x_var <- renderUI({
    data <- selected_data()
    choices <- names(data)
    
    # Default selection based on dataset type
    if(input$dataset == "Time Series") {
      selected <- "date"
    } else {
      selected <- choices[1]
    }
    
    selectInput("x", "X-axis variable:", choices = choices, selected = selected)
  })
  
  output$y_var <- renderUI({
    data <- selected_data()
    choices <- names(data)
    
    # Default selection based on dataset type
    if(input$dataset == "Time Series") {
      selected <- "value"
    } else {
      selected <- choices[2]
    }
    
    selectInput("y", "Y-axis variable:", choices = choices, selected = selected)
  })
  
  output$facet_var <- renderUI({
    # Only show factor variables for faceting
    data <- selected_data()
    factor_vars <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
    
    # Default selection based on dataset type
    if(input$dataset == "Time Series") {
      selected <- "individual"
    } else if(length(factor_vars) > 0) {
      selected <- factor_vars[1]
    } else {
      selected <- "None"
      factor_vars <- c("None")
    }
    
    selectInput("facet", "Facet by:", choices = factor_vars, selected = selected)
  })
  
  # Create the small multiples plot
  output$smallMultiples <- renderPlot({
    req(input$x, input$y, input$facet)
    
    data <- selected_data()
    
    # For time series data, calculate group average
    group_avg <- NULL
    if(input$dataset == "Time Series" && input$show_average) {
      group_avg <- data %>%
        group_by(date) %>%
        summarize(avg_value = mean(value, na.rm = TRUE))
    }
    
    # Basic plot
    if(input$dataset == "Time Series") {
      # Time series plot
      p <- ggplot(data, aes_string(x = input$x, y = input$y))
      
      if(input$line_type == "line") {
        p <- p + geom_line(aes(color = individual), size = 1)
      } else if(input$line_type == "line_point") {
        p <- p + geom_line(aes(color = individual), size = 1) + 
          geom_point(aes(color = individual), size = 2, alpha = 0.7)
      }
      
      # Add group average as reference line if requested
      if(input$show_average && !is.null(group_avg)) {
        p <- p + geom_line(data = group_avg, 
                           aes(x = date, y = avg_value), 
                           linetype = "dashed", 
                           color = "red", 
                           size = 1.2)
      }
      
      # Format x-axis for dates
      p <- p + scale_x_date(date_labels = "%b", date_breaks = "2 months") +
        labs(title = "Time Series Data by Individual", 
             x = "Date", 
             y = "Value") +
        theme_minimal() +
        theme(legend.position = "none",
              plot.title = element_text(hjust = 0.5),
              strip.background = element_rect(fill = "lightblue", colour = "black"),
              strip.text = element_text(face = "bold"))
    } else {
      # Regular scatter plot for other datasets
      p <- ggplot(data, aes_string(x = input$x, y = input$y)) +
        geom_point(alpha = 0.7) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5),
              strip.background = element_rect(fill = "lightblue", colour = "black"),
              strip.text = element_text(face = "bold"))
    }
    
    # Add faceting if a facet variable is selected and it's not "None"
    if(input$facet != "None") {
      p <- p + facet_wrap(as.formula(paste("~", input$facet)), ncol = input$ncol)
    }
    
    p
  })
}

# Run the app
shinyApp(ui, server)

## prototype app

library(tidyverse)
library(shiny)
library(bslib)
library(ggplot2)
library(lubridate)

# setwd("prototype-app")

tobacco_dept <- read_csv("tobacco_dept.csv")
tobacco_staff <- read_csv("tobacco_staff.csv")

ui <- page_fillable(
  title = "GW Tobacco History Dashboard",
  
  # Title
  div(
    h2("Explore % of Visits with a Complete Tobacco History at GW", 
       style = "text-align: center; margin-bottom: 20px;")
  ),
  
  # Top panel with controls
  card(
    card_body(
      fluidRow(
        column(6,
               selectInput("time_interval", 
                           "Please select a time interval for the visualizations, by:",
                           choices = c("Week" = "week", "Month" = "month"),
                           selected = "week")
        ),
        column(6,
               uiOutput("time_range_slider")
        )
      )
    ),
    style = "margin-bottom: 20px;"
  ),
  
  # Main content layout
  layout_columns(
    col_widths = c(8, 4),
    
    # Large bar graph
    card(
      card_header("Tobacco History Completion by Department Pilot Status"),
      card_body(
        plotOutput("main_plot", height = "400px")
      )
    ),
    
    # Two smaller visualizations
    layout_columns(
      col_widths = c(12, 12),
      
      card(
        card_header("Placeholder Visualization 1"),
        card_body(
          div("Static visualization placeholder", 
              style = "text-align: center; padding: 50px; background-color: #f8f9fa; border: 2px dashed #dee2e6;")
        )
      ),
      
      card(
        card_header("Placeholder Visualization 2"),
        card_body(
          div("Static visualization placeholder", 
              style = "text-align: center; padding: 50px; background-color: #f8f9fa; border: 2px dashed #dee2e6;")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Dynamic slider based on time interval selection
  output$time_range_slider <- renderUI({
    if(input$time_interval == "week") {
      week_range <- range(tobacco_dept$week_num, na.rm = TRUE)
      sliderInput("time_range", 
                  "Select week range:",
                  min = week_range[1], 
                  max = week_range[2],
                  value = week_range,
                  step = 1)
    } else {
      month_range <- range(tobacco_dept$month_num, na.rm = TRUE)
      sliderInput("time_range",
                  "Select month range:",
                  min = month_range[1],
                  max = month_range[2], 
                  value = month_range,
                  step = 1)
    }
  })
  
  # Filtered data based on inputs
  filtered_data <- reactive({
    req(input$time_range)
    
    if(input$time_interval == "week") {
      tobacco_dept %>%
        filter(week_num >= input$time_range[1] & week_num <= input$time_range[2])
    } else {
      tobacco_dept %>%
        filter(month_num >= input$time_range[1] & month_num <= input$time_range[2])
    }
  })
  
  # Main bar plot
  output$main_plot <- renderPlot({
    req(filtered_data())
    
    # Prepare data for plotting
    if(input$time_interval == "week") {
      plot_data <- filtered_data() %>%
        group_by(week_date, pilot_dept) %>%
        select(week_date, pilot_dept, pilot_dept_wk_compl) %>%
        mutate(date_label = format(week_date, "%a-%d")) %>% 
        rename(completion_rate = pilot_dept_wk_compl)
      
      x_var <- "date_label"
      x_label <- "Week (Mon-DD)"
      title_interval <- "Week"
    } else {
      plot_data <- filtered_data() %>%
        group_by(month_num, pilot_dept) %>%
        select(month_num, month_name, pilot_dept, pilot_dept_mo_compl) %>%
        arrange(month_num) %>%
        mutate(date_label = month_name) %>% 
        rename(completion_rate = pilot_dept_mo_compl)
      
      x_var <- "date_label" 
      x_label <- "Month"
      title_interval <- "Month"
    }
    
    # Create the bar plot
    ggplot(plot_data, aes(x = .data[[x_var]], y = completion_rate, fill = pilot_dept)) +
      geom_col(position = "dodge", alpha = 0.8) +
      scale_fill_manual(values = c("Pilot" = "#3c5a97", "Non-Pilot" = "#efca74"),
                        name = "Department Type") +
      labs(
        title = paste("Tobacco History Completion Rate by", title_interval),
        x = x_label,
        y = "Completion Rate (%)"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)
      ) +
      ylim(0, 100)
  })
}

shinyApp(ui = ui, server = server)

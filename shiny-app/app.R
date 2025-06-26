## final project app


##### global.R #####

library(tidyverse) # includes: ggplot2, dplyr, tidyr, readr, purr, tibble, stringr, forcats, lubridate
library(shiny)
library(bslib)
library(ggplot2)
library(lubridate)

tobacco_dept <- read_csv("app-data/tobacco_dept.csv")
tobacco_staff <- read_csv("app-data/tobacco_staff.csv")

#####


##### ui.R #####

ui <- page_fillable(
  
  title = "GW Tobacco History Initiative Dashboard",  # FIXME: does not appear in app -- will this be the webpage title?
  
  # TITLE (main) - centered at top
  div(
    h2("Explore % of Visits with a Complete Tobacco History at GW",
       style = "text-align: center; margin-bottom: 10px;")
  ),
  
  # TOP PANEL (instr & global controls/inputs)
  card(
    card_body(
      fluidRow(
        # INSTRUCTIONS - ~50% of the panel & left-aligned
        column(7,
               div(
                 h4("Instructions", style = "margin-bottom: 10px; font-size: 18px;"),
                 tags$ul(style = "font-size: 15px;",
                         tags$li("Select a time interval (week or month)."),
                         tags$li("Adjust the time range slider to focus on specific periods (default is the entire available data period)."),
                         tags$li("Choose whether you want to view data by Department or Staff. Both allow you to see aggregated Pilot v Non-Pilot groups."),
                         tags$li("The chart shows completion rates for all visits in Pilot v Non-Pilot groups based on your selections over time."),
                         tags$li("The table displays the 10 Departments/Staff with the highest complete tobacco history rates over the selected time range.",
                                 tags$br(),
                                 "(averages the % complete for the selected intervals over the selected time range)"),
                         tags$br(),
                         tags$li(tags$b("The Tobacco History Initiative Pilot began on 03/21/25.")),  # bold
                         tags$i("* indicates the Department/Staff is part of the Pilot group."))  # italics
               )  # close div()
        ),  # close INSTRUCTIONS column()
        
        # SPACER
        #column(1),
        
        # TIME INPUTS - centered
        column(4,
               div(style = "text-align: center;",
                   p("Use the controls to customize your view:", style = "font-size: 16px;"),
                   # SELECT INPUTS (1st row)
                   fluidRow(
                     column(6,
                            selectInput("time_interval", 
                                        "View time interval by:",
                                        choices = c("Week" = "week", "Month" = "month"),
                                        selected = "week")
                     ),
                     column(6,
                            selectInput("dept_staff", 
                                        "View by Department or Staff:",
                                        choices = c("Department" = "dept", "Staff" = "staff"),
                                        selected = "dept")
                     )
                   ),
                   # SLIDER INPUT (2nd row)
                   fluidRow(
                     column(2),  # SPACER
                     column(8,   # SLIDER
                            uiOutput("time_range_slider")  # NOTE that this is a dynamic UI output based on selected time interval & so is in the server
                     ),
                     column(2)   # SPACER
                   )
               )  # close div()
        ),  # close TIME INPUTS column()
        
        # SPACER
        column(1)
        
      )  # close fluidRow()
    ),  # close card_body()
    style = "margin-bottom: 10px;"
  ),  # close TOP PANEL card()
  
  # MAIN CONTENT PANEL - split evenly
  # TODO: depending on width of graph, give a little more space to graph over table?
  layout_columns(
    col_widths = c(6, 6),  # TODO: add another row later with the 2 interactive line plots?
    
    # BAR GRAPH
    card(card_header(uiOutput("plot_title")),
         card_body(plotOutput("main_plot", height = "400px"))),
    
    # TOP 10 TABLE
    card(card_header(uiOutput("table_title")),
         card_body(DT::dataTableOutput("top_performers_table")))
    
  )  # close layout_columns()
  
)  # close page_fillable() (and ui)

#####


##### server.R #####

server <- function(input, output, session) {
  
  # get dataset based on [dept/staff] selection
  current_data <- reactive({
    if(input$dept_staff == "dept") {
      tobacco_dept
    } else {
      tobacco_staff
    }
  })
  
  # TIME RANGE SLIDER INPUT
  # dynamic slider based on [week/month] time interval selection and [dept/staff] dataset
  output$time_range_slider <- renderUI({
    data <- current_data()
    
    if(input$time_interval == "week") {
      week_range <- range(data$week_num, na.rm = TRUE)
      sliderInput("time_range", 
                  "Select week range:",
                  min = week_range[1], 
                  max = week_range[2],
                  value = week_range,
                  step = 1)  # TODO: check labels (prefer week_date formatted as Mon-DD)
    } else {
      month_range <- range(data$month_num, na.rm = TRUE)
      sliderInput("time_range",
                  "Select month range:",
                  min = month_range[1],
                  max = month_range[2], 
                  value = month_range,
                  step = 1)  # TODO: check labels (prefer month_name)
    }
  })
  
  # filter dataset based on time range selection
  filtered_data <- reactive({
    req(input$time_range)
    data <- current_data()
    
    if(input$time_interval == "week") {
      data %>%
        filter(week_num >= input$time_range[1] & week_num <= input$time_range[2])
    } else {
      data %>%
        filter(month_num >= input$time_range[1] & month_num <= input$time_range[2])
    }
  })
  
  # DYNAMIC TITLING
  
  output$current_interval <- renderText({
    if(input$time_interval == "week") "Weekly" else "Monthly"
  })
  
  output$current_time_range <- renderText({
    req(input$time_range)
    paste("Range:", input$time_range[1], "to", input$time_range[2])
  })
  
  # PILOT BAR GRAPH TITLE
  output$plot_title <- renderUI({
    interval_type <- if(input$time_interval == "week") "Weekly" else "Monthly"
    view_type <- if(input$dept_staff == "dept") "Department" else "Staff"  # TODO: will need to update once can select multiple depts/staff
    h4(paste(interval_type, "Percent of Visits with Complete Tobacco History by", view_type))
  })
  
  # PILOT BAR GRAPH
  output$main_plot <- renderPlot({
    req(filtered_data())
    
    if(input$dept_staff == "dept") {
      # PILOT DEPT - WEEK
      if(input$time_interval == "week") {
        plot_data <- filtered_data() %>%  # TODO: will need to add code to handle additional/changed user selection of depts once multi-selectize input added
          group_by(week_num, pilot_dept) %>%
          select(week_num, week_date, pilot_dept, pilot_dept_wk_compl) %>%
          arrange(week_num) %>%
          rename(completion_rate = pilot_dept_wk_compl)
        
        x_var <- "week_date"
        x_label <- "Week of"
        date_label <- "%b-%d"
        date_break <- "1 week"
        title_interval <- "Week"
        fill_var <- "pilot_dept"  # TODO: will need to add code to handle additional/changed user selection of depts once multi-selectize input added
        legend_name <- "Pilot Status"  # TODO: remove legend title for graphs with only Pilot/Non-Pilot?
        x_axis_angle <- 45  # tilted x-axis labels for WEEK
        x_axis_text_size <- 14  # smaller text font size for large number of WEEK x-axis labels
        x_axis_title_size <- 18 # smaller title font size for large number of WEEK x-axis labels
        n_periods <- length(unique(plot_data[[x_var]]))
        bar_width <- pmax(5, pmin(10, 1 - (n_periods * 0.05)))  # dynamic width between 0.3 and 0.8 -> 5:10
        dodge_width <- bar_width + 0.3  # Add spacing between bar pairs
      } else {
        # PILOT DEPT - MONTH
        plot_data <- filtered_data() %>%  # TODO: will need to add code to handle additional/changed user selection of depts once multi-selectize input added
          group_by(month_num, pilot_dept) %>%
          select(month_num, month_date, month_name, pilot_dept, pilot_dept_mo_compl) %>%
          arrange(month_num) %>%
          rename(completion_rate = pilot_dept_mo_compl)
        
        x_var <- "month_date" 
        x_label <- "Month"
        date_label <- "%b"
        date_break <- "1 month"
        title_interval <- "Month"
        fill_var <- "pilot_dept"  # TODO: will need to add code to handle additional/changed user selection of depts once multi-selectize input added
        legend_name <- "Pilot Status"  # TODO: remove legend title for graphs with only Pilot/Non-Pilot?
        x_axis_angle <- 0  # horizontal (normal) x-axis labels for MONTH
        x_axis_text_size <- 18  # larger text font size for horizontal/fewer MONTH x-axis labels
        x_axis_title_size <- 22 # larger title font size for horizontal/fewer MONTH x-axis labels
        n_periods <- length(unique(plot_data[[x_var]]))
        bar_width <- pmax(25, pmin(45, 1 - (n_periods * 0.05)))  # dynamic width between 0.3 and 0.8 -> 25:45
        dodge_width <- bar_width + 0.3  # Add spacing between bar pairs
      }
    } else {
      # PILOT STAFF - WEEK
      if(input$time_interval == "week") {
        plot_data <- filtered_data() %>%  # TODO: will need to add code to handle additional/changed user selection of depts once multi-selectize input added
          group_by(week_num, pilot_staff) %>%
          select(week_num, week_date, pilot_staff, pilot_staff_wk_compl) %>%
          arrange(week_num) %>%
          rename(completion_rate = pilot_staff_wk_compl)
        
        x_var <- "week_date"
        x_label <- "Week of"
        date_label <- "%b-%d"
        date_break <- "1 week"
        title_interval <- "Week"
        fill_var <- "pilot_staff"  # TODO: will need to add code to handle additional/changed user selection of depts once multi-selectize input added
        legend_name <- "Pilot Status"  # TODO: remove legend title for graphs with only Pilot/Non-Pilot?
        x_axis_angle <- 45  # tilted x-axis labels for WEEK
        x_axis_text_size <- 14  # smaller text font size for large number of WEEK x-axis labels
        x_axis_title_size <- 18 # smaller title font size for large number of WEEK x-axis labels
        n_periods <- length(unique(plot_data[[x_var]]))
        bar_width <- pmax(5, pmin(10, 1 - (n_periods * 0.05)))  # dynamic width between 0.3 and 0.8 -> 5:10
        dodge_width <- bar_width + 0.3  # Add spacing between bar pairs
      } else {
        # PILOT STAFF - MONTH
        plot_data <- filtered_data() %>%  # TODO: will need to add code to handle additional/changed user selection of depts once multi-selectize input added
          group_by(month_num, pilot_staff) %>%
          select(month_num, month_date, month_name, pilot_staff, pilot_staff_mo_compl) %>%
          arrange(month_num) %>%
          rename(completion_rate = pilot_staff_mo_compl)
        
        x_var <- "month_date"
        x_label <- "Month"
        date_label <- "%b"
        date_break <- "1 month"
        title_interval <- "Month"
        fill_var <- "pilot_staff"  # TODO: will need to add code to handle additional/changed user selection of depts once multi-selectize input added
        legend_name <- "Pilot Status"  # TODO: remove legend title for graphs with only Pilot/Non-Pilot?
        x_axis_angle <- 0  # horizontal (normal) x-axis labels for MONTH
        x_axis_text_size <- 18  # larger text font size for horizontal/fewer MONTH x-axis labels
        x_axis_title_size <- 22 # larger title font size for horizontal/fewer MONTH x-axis labels
        n_periods <- length(unique(plot_data[[x_var]]))
        bar_width <- pmax(25, pmin(45, 1 - (n_periods * 0.05)))  # dynamic width between 0.3 and 0.8 -> 25:45
        dodge_width <- bar_width + 0.3  # Add spacing between bar pairs
      }
    }
    
    # PLOT BAR GRAPH
    ggplot(plot_data, aes(x = .data[[x_var]], y = completion_rate, fill = .data[[fill_var]])) +
      #geom_bar(stat = "identity", position = "dodge") + 
      # does not work with geom_bar, only with geom_col (at least with all other code same)
      geom_col(position = position_dodge(width = dodge_width), 
               width = bar_width,
               alpha = 0.8) +  # (Shiny Assistant said default width = 0.9 but 0.7 was SKINNY, 4-6 was good for weeks) --> smaller = more space bet/ bars
      labs(x = x_label, y = "% Complete", 
           fill = legend_name) +
      theme_minimal() +
      scale_fill_manual(values = c("Pilot" = "#204182", "Non-Pilot" = "#e0b85a")) +
      scale_x_date(date_labels = date_label, date_breaks = date_break) +
      theme(axis.text.x = element_text(angle = x_axis_angle, hjust = if(x_axis_angle > 0) 1 else 0.5, size = x_axis_text_size),
            axis.title.x = element_text(size = x_axis_title_size, face = "bold", margin = margin(t = 15)),  # bold x-axis title with more space
            axis.text.y = element_text(size = 16),
            axis.title.y = element_text(size = 18),
            legend.text = element_text(size = 16),
            legend.title = element_text(size = 16),
            legend.position = "bottom") +  # TODO: move to top (top-right?) overlaid on the graph?
      #ylim(50, 100)
      coord_cartesian(ylim = c(50, 100))  # TODO: add a vertical line to the graph(s) at pilot start date if incl in selected time range
  })
  
  # TOP 10 TABLE TITLE
  output$table_title <- renderUI({
    view_type <- if(input$dept_staff == "dept") "Departments" else "Staff"
    h4(paste("Top 10 Performing", view_type))
  })
  
  # TOP 10 TABLE
  output$top_performers_table <- DT::renderDataTable({
    
    req(filtered_data())
    
    # TODO: later add a column "Total Visits" for total # of visits in that dept over the time range
    # TOP 10 - DEPT
    if(input$dept_staff == "dept") {
      # DEPT top 10 - WEEK
      # TODO: currently shows spec (grouped specialties), later show by raw dept?
      if(input$time_interval == "week") {
        top_data <- filtered_data() %>%
          group_by(spec_p) %>%
          summarise(avg_completion = mean(spec_wk_compl, na.rm = TRUE), .groups = 'drop') %>%
          arrange(desc(avg_completion)) %>%
          # use min_rank() to handle ties properly - departments with same % get same rank
          mutate(Rank = min_rank(desc(avg_completion)),
                 avg_completion = sprintf("%.1f", avg_completion)) %>%  # format as ##.#
          # filter to top 10 ranks (may include more than 10 rows if there are ties)
          filter(Rank <= 10) %>%
          select(Rank, Specialty = spec_p, `% Complete` = avg_completion) %>%
          arrange(Rank)
        # slice_head(n = 10) %>%
        # mutate(Rank = row_number(),  
        # FIXME: need to adjust to account for ties (e.g. if 2 have 100%, they should both be rank #1)
        #        avg_completion = sprintf("%.1f", avg_completion)) %>%  # format as ##.#
        # select(Rank, Specialty = spec_p, `% Complete` = avg_completion)
      } else {
        # DEPT top 10 - MONTH
        top_data <- filtered_data() %>%
          group_by(spec_p) %>%
          summarise(avg_completion = mean(spec_mo_compl, na.rm = TRUE), .groups = 'drop') %>%
          arrange(desc(avg_completion)) %>%
          # use min_rank() to handle ties properly - departments with same % get same rank
          mutate(Rank = min_rank(desc(avg_completion)),
                 avg_completion = sprintf("%.1f", avg_completion)) %>%  # format as ##.#
          # filter to top 10 ranks (may include more than 10 rows if there are ties)
          filter(Rank <= 10) %>%
          select(Rank, Specialty = spec_p, `% Complete` = avg_completion) %>%
          arrange(Rank)
        # slice_head(n = 10) %>%
        # mutate(Rank = row_number(),  
        # FIXME: need to adjust to account for ties (e.g. if 2 have 100%, they should both be rank #1)
        #        avg_completion = sprintf("%.1f", avg_completion)) %>%  # format as ##.#
        # select(Rank, Specialty = spec_p, `% Complete` = avg_completion)
      }
    } else {  # close if (dept) for dept/staff selection for TOP 10 TABLE
      # TOP 10 - STAFF
      # STAFF top 10 - WEEK
      if(input$time_interval == "week") {
        top_data <- filtered_data() %>%
          group_by(staff_ID_p) %>%
          summarise(avg_completion = mean(staff_wk_compl, na.rm = TRUE), .groups = 'drop') %>%
          arrange(desc(avg_completion)) %>%
          # use min_rank() to handle ties properly - departments with same % get same rank
          mutate(Rank = min_rank(desc(avg_completion)),
                 avg_completion = sprintf("%.1f", avg_completion)) %>%  # format as ##.#
          # filter to top 10 ranks (may include more than 10 rows if there are ties)
          filter(Rank <= 10) %>%
          select(Rank, `Staff ID` = staff_ID_p, `% Complete` = avg_completion) %>%
          arrange(Rank)
        # slice_head(n = 10) %>%  
        # FIXME: may need to change to allow more entries as >10 have 100%
        # mutate(Rank = row_number(),  
        # FIXME: need to adjust to account for ties (e.g. if 2 have 100%, they should both be rank #1)
        #        avg_completion = sprintf("%.1f", avg_completion)) %>%  # format as ##.#
        # select(Rank, `Staff ID` = staff_ID_p, `% Complete` = avg_completion)
      } else {
        # STAFF top 10 - MONTH
        top_data <- filtered_data() %>%
          group_by(staff_ID_p) %>%
          summarise(
            avg_completion = mean(staff_mo_compl, na.rm = TRUE),
            .groups = 'drop'
          ) %>%
          arrange(desc(avg_completion)) %>%
          # use min_rank() to handle ties properly - departments with same % get same rank
          mutate(Rank = min_rank(desc(avg_completion)),
                 avg_completion = sprintf("%.1f", avg_completion)) %>%  # format as ##.#
          # filter to top 10 ranks (may include more than 10 rows if there are ties)
          filter(Rank <= 10) %>%
          select(Rank, `Staff ID` = staff_ID_p, `% Complete` = avg_completion) %>%
          arrange(Rank)
        # slice_head(n = 10) %>%  
        # FIXME: may need to change to allow more entries as >10 have 100%
        # mutate(Rank = row_number(),  
        # FIXME: need to adjust to account for ties (e.g. if 2 have 100%, they should both be rank #1)
        #        avg_completion = sprintf("%.1f", avg_completion)) %>%  # format as ##.#
        # select(Rank, `Staff ID` = staff_ID_p, `% Complete` = avg_completion)
      }
    }  # close else (staff) for dept/staff selection for TOP 10 TABLE
    
    names(data)
    DT::datatable(
      top_data,
      options = list(pageLength = 10,
                     dom = 't',
                     ordering = FALSE,
                     searching = FALSE,
                     columnDefs = list(list(className = 'dt-center', targets = 0),  # centers Rank and Total Visits columns
                                       list(className = 'dt-right', targets = 2)  # right-aligns % Complete column
                     )
      ),
      rownames = FALSE
    )
    
  })  # close renderDataTable()
  
}  # close server function

#####

shinyApp(ui, server)

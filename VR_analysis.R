if (!require(shiny)) install.packages("shiny", dependencies = TRUE)
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)
if (!require(dplyr)) install.packages("dplyr", dependencies = TRUE)
if (!require(ggridges)) install.packages("ggridges", dependencies = TRUE)
if (!require(shinythemes)) install.packages("shinythemes", dependencies = TRUE)


library(shiny)
library(dplyr)
library(ggplot2)
library(shinythemes)
library(ggridges) 

# Read data and ensure numeric type for VR hours
data_url <- "https://raw.githubusercontent.com/Aaron00Yu94/Impact-of-Virtual-Reality-on-Education/main/Virtual_Reality_in_Education_Impact.csv"
data <- read.csv(data_url)
data$Hours_of_VR_Usage_Per_Week <- as.numeric(data$Hours_of_VR_Usage_Per_Week)
data <- data %>% filter(!is.na(Hours_of_VR_Usage_Per_Week))  # Remove missing values

# Mapping perceived effectiveness levels to descriptions
effectiveness_labels <- c(
  "1 (not that helpful)", 
  "2", 
  "3", 
  "4", 
  "5 (very helpful)"
)

# Helper function to create ridges plot
create_ridges_plot <- function(data) {
  ggplot(data, aes(x = Perceived_Effectiveness_of_VR, 
                   y = Field_of_Study, 
                   fill = ..x..)) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
    scale_fill_viridis_c(name = "VR Effectiveness", option = "C") +
    labs(title = "Distribution of VR Perceived Effectiveness across Fields",
         x = "Perceived Effectiveness of VR",
         y = "Field of Study") +
    theme_ridges() +
    theme(
      plot.title = element_text(face = "bold", size = 18, color = "#2c3e50"),
      axis.title = element_text(face = "bold", size = 14, color = "#34495e"),
      axis.text = element_text(size = 12, color = "#2c3e50")
    )
}

# Helper function to filter data based on VR usage range
filter_data_by_vr_hours <- function(data, vr_hours) {
  data %>%
    filter(Hours_of_VR_Usage_Per_Week >= vr_hours[1],
           Hours_of_VR_Usage_Per_Week <= vr_hours[2]) %>%
    group_by(Improvement_in_Learning_Outcomes) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    mutate(Percentage = Count / sum(Count) * 100)
}

# Helper function to create improvement in learning outcomes plot
create_outcome_plot <- function(filtered_data) {
  ggplot(filtered_data, aes(x = Improvement_in_Learning_Outcomes, 
                            y = Percentage, 
                            fill = Improvement_in_Learning_Outcomes)) +
    geom_bar(stat = "identity", width = 0.5) +
    scale_fill_manual(values = c("#2ecc71", "#e74c3c")) +
    labs(title = "VR Usage and Learning Outcome Improvement",
         x = "Improvement in Learning Outcomes",
         y = "Percentage",
         fill = "Outcome") +
    theme_minimal(base_family = "Arial") +
    theme(
      plot.title = element_text(face = "bold", size = 18, color = "#2c3e50"),
      axis.title = element_text(face = "bold", size = 14, color = "#34495e"),
      axis.text = element_text(size = 12, color = "#2c3e50"),
      legend.title = element_text(face = "bold", color = "#2c3e50"),
      legend.position = "top"
    )
}

# Helper function to create field of study plot
create_field_plot <- function(data) {
  field_data <- data %>%
    group_by(Field_of_Study, Perceived_Effectiveness_of_VR) %>%
    summarise(Count = n(), .groups = 'drop')
  
  ggplot(field_data, aes(x = Field_of_Study, 
                         y = Count, 
                         fill = as.factor(Perceived_Effectiveness_of_VR))) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_brewer(palette = "Blues", name = "Effectiveness Level") +
    labs(title = "Field of Study and Perceived Effectiveness of VR",
         x = "Field of Study",
         y = "Total Students") +
    theme_minimal(base_family = "Arial") +
    theme(
      plot.title = element_text(face = "bold", size = 18, color = "#2c3e50"),
      axis.title = element_text(face = "bold", size = 14, color = "#34495e"),
      axis.text = element_text(size = 12, angle = 45, hjust = 1, color = "#2c3e50"),
      legend.title = element_text(face = "bold", color = "#2c3e50"),
      legend.position = "top"
    )
}

# Helper function to generate field-specific perceived effectiveness text
generate_field_info_text <- function(data, field_selected, effectiveness_labels) {
  field_info <- data %>%
    filter(Field_of_Study == field_selected) %>%
    group_by(Perceived_Effectiveness_of_VR) %>%
    summarise(Count = n(), .groups = 'drop')
  
  effectiveness_text <- paste0(
    effectiveness_labels[field_info$Perceived_Effectiveness_of_VR], 
    ": ", field_info$Count, " students"
  )
  
  paste0("In the field of <b>", field_selected, "</b>, perceived effectiveness is as follows:<br>",
         paste(effectiveness_text, collapse = "<br>"))
}


# UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel(
    div("Impact of VR Usage Hours on Learning Outcomes", 
        style = "font-weight: bold; font-size: 24px; color: #2c3e50; text-align: center; margin-bottom: 20px;")
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("graph_choice", 
                  "Select Visualization:",
                  choices = c("VR Usage and Learning Outcome Improvement", 
                              "Field of Study and VR Perception",
                              "VR Usage vs Field of Study")), 
      br(),
      sliderInput("vr_hours",
                  "Select Range of Weekly VR Usage Hours:",
                  min = min(data$Hours_of_VR_Usage_Per_Week, na.rm = TRUE),
                  max = max(data$Hours_of_VR_Usage_Per_Week, na.rm = TRUE),
                  value = c(min(data$Hours_of_VR_Usage_Per_Week, na.rm = TRUE), 
                            max(data$Hours_of_VR_Usage_Per_Week, na.rm = TRUE))
      ),
      helpText("Drag the slider to select the range of weekly VR usage hours.", 
               style = "font-size: 16px; color: #34495e;")
    ),
    mainPanel(
      uiOutput("dynamicPlot"),  
      br(),
      conditionalPanel(
        condition = "input.graph_choice == 'VR Usage vs Field of Study'",
        plotOutput("ridgesPlot")  
      ),
      br(),
      div(uiOutput("field_info"), style = "font-size: 16px; color: #34495e;"),
      br(),
      uiOutput("findings_ui"),
      style = "background-color: #ecf0f1; padding: 20px; border-radius: 10px; box-shadow: 2px 2px 10px rgba(0, 0, 0, 0.1);"
    )
  )
)


server <- function(input, output) {
  # Reactive value to store clicked field information
  clicked_field <- reactiveVal(NULL)
  # Reactive value to store brushed data
  brushed_data <- reactiveVal(data)
  
  # Reset clicked_field when changing graph choice
  observeEvent(input$graph_choice, {
    clicked_field(NULL)
  })
  
  # Reactive data filtering for VR usage
  filtered_data <- reactive({
    filter_data_by_vr_hours(data, input$vr_hours)
  })
  
  # Plot improvement in learning outcomes
  output$outcomePlot <- renderPlot({
    req(filtered_data())
    create_outcome_plot(filtered_data())
  })
  
  # Plot field of study and perceived VR effectiveness
  output$fieldPlot <- renderPlot({
    create_field_plot(data)
  })
  
  # Handle clicks on the field plot
  
  # Handle clicks on the field plot
  observeEvent(input$field_click, {

    field_clicked <- input$field_click$x
    field_names <- levels(as.factor(data$Field_of_Study))  
    
    if (!is.null(field_clicked) && field_clicked > 0 && field_clicked <= length(field_names)) {
      field_index <- round(field_clicked)
      clicked_field(field_names[field_index])
    }
  })
  
  
  # Display field-specific information
  output$field_info <- renderUI({
    if (input$graph_choice == "Field of Study and VR Perception" && !is.null(clicked_field())) {
      HTML(generate_field_info_text(data, clicked_field(), effectiveness_labels))
    }
  })
  
  # Display findings based on the selected visualization
  output$findings_ui <- renderUI({
    if (input$graph_choice == "VR Usage and Learning Outcome Improvement") {
      req(filtered_data())
      selected_range <- paste(input$vr_hours[1], "to", input$vr_hours[2])
      percentage <- round(filtered_data()$Percentage[filtered_data()$Improvement_in_Learning_Outcomes == "Yes"], 2)
      
      HTML(paste0("Within the selected range of VR usage hours (", selected_range, "), ",
                  "<b style='color: red;'>", percentage, "%</b> of students reported an improvement in learning outcomes."))
    }
  })
  
  # Dynamic plot based on dropdown selection
  output$dynamicPlot <- renderUI({
    if (input$graph_choice == "VR Usage and Learning Outcome Improvement") {
      plotOutput("outcomePlot")
    } else if (input$graph_choice == "Field of Study and VR Perception") {
      plotOutput("fieldPlot", click = "field_click")
    } else if (input$graph_choice == "VR Usage vs Field of Study") {
      plotOutput("vrVsFieldPlot", brush = "scatter_brush")  
    }
  })
  
  # Plot VR Usage vs Field of Study (scatter plot)
  output$vrVsFieldPlot <- renderPlot({
    ggplot(data, aes(x = Hours_of_VR_Usage_Per_Week, 
                     y = Field_of_Study, 
                     color = Field_of_Study)) +
      geom_point(size = 3, alpha = 0.6) +
      labs(title = "VR Usage vs Field of Study",
           x = "Hours of VR Usage Per Week",
           y = "Field of Study") +
      theme_minimal(base_family = "Arial") +
      theme(
        plot.title = element_text(face = "bold", size = 18, color = "#2c3e50"),
        axis.title = element_text(face = "bold", size = 14, color = "#34495e"),
        axis.text = element_text(size = 12, color = "#2c3e50")
      )
  })
  # Plot ridges plot based on brushed data, only when 'VR Usage vs Field of Study' is selected
  output$ridgesPlot <- renderPlot({
    if (input$graph_choice == "VR Usage vs Field of Study") {
      create_ridges_plot(brushed_data())
    }
  })
  
  observeEvent(input$scatter_brush, {
    brushed_points <- brushedPoints(data, input$scatter_brush)
    if (nrow(brushed_points) > 0) {
      brushed_data(brushed_points)
    } else {
      brushed_data(data)  
    }
  })
  
  
}



# Launch the Shiny app
options(shiny.launch.browser = FALSE)
shinyApp(ui = ui, server = server)
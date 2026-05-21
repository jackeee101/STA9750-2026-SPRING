library(shiny)
library(dplyr)
library(data.table)
library(plotly)


summary_df <- readRDS("summary_df.rds")

ui <- fluidPage(
  
  titlePanel("311 Service Request Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "problem",
        "Select Problem Type:",
        choices = sort(unique(summary_df$problem_clean))
      )
    ),
    
    mainPanel(
      plotlyOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    summary_df |>
      filter(problem_clean == input$problem)
  })
  
  output$plot <- renderPlotly({
    
    plot_data <- filtered_data() |>
      group_by(`Open Data Channel Type`) |>
      summarise(
        avg_time = median(avg_time, na.rm = TRUE),
        .groups = "drop"
      )
    
    plot_ly(
      data = plot_data,
      x = ~`Open Data Channel Type`,
      y = ~avg_time,
      type = "bar"
    )
  })
}

shinyApp(ui, server)

rsconnect::deployApp("C:/Users/xchen/Downloads/shiny")


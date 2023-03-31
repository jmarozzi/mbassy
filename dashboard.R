## app.R ##
library(shiny)
library(shinydashboard)
library(nousstyle)

ui <- dashboardPage(
  dashboardHeader(title = "MBassy analysis"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(plotOutput("plot1", height = 250)),
  )))


server <- function(input, output) {
  output$plot1 <- renderPlot({
    unique_students_plot
  })
}



shinyApp(ui, server)

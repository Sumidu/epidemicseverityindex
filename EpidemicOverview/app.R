library(shinydashboard)
library(shiny)
library(corona)
ui <- dashboardPage(
    dashboardHeader(title = "European Epidemic Overview"),
    dashboardSidebar(),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        fluidRow(
            box(width = 12,
                title = "Plot",
                plotOutput("plot1")),
            box(
                title = "Controls",
                uiOutput("countryselector")
            )
        )
    )
)

server <- function(input, output) {

    output$plot1 <- renderPlot({

    })
}

shinyApp(ui, server)

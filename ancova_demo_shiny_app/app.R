#Created by Keith Post on 10/18/22
#Shiny App demo for data that fit an ANCOVA

library(shiny)

# Define UI 
ui <- fluidPage(

    # Application title
    titlePanel("ANCOVA Demo"),

    # Sidebar with 
    sidebarLayout(
        sidebarPanel(
        ),

        # Show a plot of the data
        mainPanel(
           plotOutput("scatterPlot")
        )
    )
)

# Define server function
server <- function(input, output, session) {

    output$scatterPlot <- renderPlot({
  
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

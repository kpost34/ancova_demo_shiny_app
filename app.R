#Created by Keith Post on 10/18/22
#Shiny App demo for data that fit an ANCOVA

library(shiny)
library(tidyverse)
library(janitor)

data(mtcars)

mtcars<-as_tibble(mtcars) %>%
  select(-c(cyl,gear,carb)) %>%
  mutate(across(c(vs,am),~as.factor(.x)))



# Define UI 
ui <- fluidPage(

  # Application title
  titlePanel("ANCOVA Demo"),

  # Sidebar panel layout
  sidebarLayout(
    sidebarPanel(width=4,
      varSelectInput("varSel_num","Select a continuous variable",
                     data=mtcars %>%
                       select(where(is.numeric),-mpg)),
      varSelectInput("varSel_cat","Select a binary variable",
                     data=mtcars %>%
                       select(where(is.factor))),
      checkboxGroupInput("chkGrp_regs","Select one or more regression lines",
                         choices="All data without groups")
    ),

    # Show tables and a plot of the data
    mainPanel(width=8,
      fluidRow(
        column(width=4,
          tableOutput("table1")
        ),
        column(width=4,
          tableOutput("table2")
        )
      ),
      fluidRow(
        plotOutput("scatterPlot")
      )
    )
  )
)

# Define server function
server <- function(input, output, session) {
  
  output$table1<-renderTable({
    mtcars %>%
      select(!!input$varSel_num,mpg) %>%
      summarize(across(everything(),~mean(.x)))
  })
    
  output$table2<-renderTable({
    mtcars %>%
      select(!!input$varSel_num,!!input$varSel_cat,mpg) %>%
      group_by(!!input$varSel_cat) %>%
      summarize(across(everything(),~mean(.x)))
  })
    

    output$scatterPlot <- renderPlot({
      ggplot(mtcars,aes(x=!!input$varSel_num,y=mpg,color=!!input$varSel_cat)) +
        geom_point() +
        theme_bw() -> p
      
      if(!is.null(input$chkGrp_regs)){
        p + geom_smooth(method="lm")->p
      }
      p
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

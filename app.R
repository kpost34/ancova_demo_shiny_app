#Created by Keith Post on 10/18/22
#Shiny App demo for data that fit an ANCOVA

#load packages
pacman::p_load(shiny,tidyverse,janitor,plotly,broom)

#load data
data(mtcars)

#wrangle data
mtcars<-as_tibble(mtcars,rownames="model") %>%
  select(-c(cyl,gear,carb)) %>%
  mutate(across(c(vs,am),~as.factor(.x)))



#### Define UI 
ui <- fluidPage(
  
  ### CSS to make horizontal line thicker
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),

  ### Application title
  titlePanel("ANCOVA Demo"),

  #### Sidebar panel layout
  sidebarLayout(
    sidebarPanel(width=3,
      selectInput("sel_num","Select a continuous variable",
                  choices=mtcars %>%
                    select(where(is.numeric),-mpg) %>%
                    names()),
      selectInput("sel_cat","Select a binary variable",
                  choices=mtcars %>%
                    select(where(is.factor)) %>%
                    names()),
      checkboxGroupInput("chkGrp_regs","Select regression line(s)",
                         choices=c("No groups"="all","Grouped by binary variable"="by_bi"))
    ),

    #### Show tables and a plot of the data
    mainPanel(width=9,
      fluidRow(
        column(4,
          tableOutput("sum_tab_noGroup")
        ),
        column(2),
        column(3,
          tableOutput("sum_tab_wGroup")
        )
      ),
      hr(),
      fluidRow(
        column(5,
          plotlyOutput("scatter_plot")
        ),
        column(1),
        column(3,
          tableOutput("mod_tab_noGroup"),
          br(),
          tableOutput("mod_tab_wGroup")
        )
      )
    )
  )
)

#### Define server function
server <- function(input, output, session) {
  
  ### Output tables using selected variables
  ## No groups
  output$sum_tab_noGroup<-renderTable({
    mtcars %>%
      select(!!sym(input$sel_num),mpg) %>%
      summarize(across(everything(),list(mean=mean,sd=sd)))},
    striped=TRUE,hover=TRUE,caption="Without groups"
  )
    
  ## Grouped by categorical variable
  output$sum_tab_wGroup<-renderTable({
    mtcars %>%
      select(!!sym(input$sel_num),!!sym(input$sel_cat),mpg) %>%
      group_by(!!sym(input$sel_cat)) %>%
      summarize(across(everything(),list(mean=mean,sd=sd)))},
    striped=TRUE,hover=TRUE,caption="Grouped by binary variable"
  )

  ### Output scatterplot
  ## ggplot
  output$scatter_plot <- renderPlotly({
    mtcars %>%
      ggplot(aes(label=model)) +
      geom_point(aes(x=!!sym(input$sel_num),y=mpg,color=!!sym(input$sel_cat)),size=2,alpha=0.7) +
      scale_color_viridis_d() +
      theme_bw() +
      theme(text=element_text(size=16)) -> p1
      
    ## add smoother using checkbox group input
      if(!is.null(input$chkGrp_regs)){
        if(length(input$chkGrp_regs)==2){
          p1 + 
            geom_smooth(aes(x=!!sym(input$sel_num),y=mpg), method="lm",se=FALSE) +
            geom_smooth(aes(x=!!sym(input$sel_num),y=mpg,color=!!sym(input$sel_cat)),method="lm",se=FALSE) -> p2
        }
        else if(input$chkGrp_regs=="all"){
          p1 + 
            geom_smooth(aes(x=!!sym(input$sel_num),y=mpg), method="lm",se=FALSE) -> p2
        }
        else if(input$chkGrp_regs=="by_bi"){
          p1 + 
            geom_smooth(aes(x=!!sym(input$sel_num),y=mpg,color=!!sym(input$sel_cat)),method="lm",se=FALSE) -> p2
        }
      }
    
    else{p1->p2}
    
    ## turn ggplot object into a plotly
    ggplotly(p2) %>% 
      # put legend centered underneath plot
      layout(legend=list(
        orientation="h",xanchor="center",x=0.5,y=-0.3))
  })
  
  
  ### Create reactive expressions of models
  ## Without groups
  mod1<-reactive({
    req(input$chkGrp_regs=="all")
    mtcars %>%
      lm(paste0("mpg","~",input$sel_num) %>% as.formula(),data=.)
  })
  
  ## With groups
  mod2<-reactive({
    req(input$chkGrp_regs=="by_bi")
    mtcars %>%
      lm(paste0("mpg","~",input$sel_num,"+",input$sel_cat) %>% as.formula(),data=.)
  })
  
  
  ### Write output
  ## Without groups
  output$mod_tab_noGroup<-renderTable({
      mod1() %>%
        tidy()
    })
  
  ## With groups
  output$mod_tab_wGroup<-renderTable({
    mod2() %>%
      tidy()
  })
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)



## Done
## got functionality of smoothers to work
## improved layout
## turned scatter plot into a plotly


## Last commit
## added code to produce tables, a scatter plot, and a smoother


# mod1<-lm(mpg~disp,data=mtcars)
# summary(mod1)[[4]][,1]
# summary(mod1)[[8]]


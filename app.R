#Created by Keith Post on 10/18/22
#Shiny App demo for data that fit an ANCOVA


#load packages
pacman::p_load(shiny,here,tidyverse,janitor,plotly,broom)


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
    sidebarPanel(width=2,
      selectInput("sel_num","Select a continuous variable",
                  choices=mtcars %>%
                    select(where(is.numeric),-mpg) %>%
                    names()),
      selectInput("sel_cat","Select a binary variable",
                  choices=mtcars %>%
                    select(where(is.factor)) %>%
                    names()),
      checkboxGroupInput("chkGrp_regs","Select regression line(s)",
                         choices=c("No groups"="all","Grouped by binary variable"="by_bi")),
      checkboxGroupInput("chkGrp_ancova","Run ANCOVA on",
                         choices=c("Full model with interaction"="full_mod_wIntxn"))
    ),

    #### Show tables and a plot of the data
    mainPanel(width=10,
      #summary stats in tables
      fluidRow(
        column(4,
          tableOutput("sum_tab_noGroup")
        ),
        column(1),
        column(5,
          tableOutput("sum_tab_wGroup")
        )
      ),
      #add horizontal line
      hr(),
      #interactive plot
      fluidRow(
        column(5,
          plotlyOutput("scatter_plot")
        ),
        #add vertical space between plot and table(s)
        # column(1),
        #model outputs
        column(5,
          tableOutput("mod1_tab_noGroup"),
          br(),
          splitLayout(
            tableOutput("mod2a_tab_wGroup"),
            tableOutput("mod2b_tab_wGroup")
            
          )
        )
      ),
      #add second horizontal line
      hr(),
      #anova table
      fluidRow(
        column(5,
          tableOutput("ancova_sum_tab")
        ),
        column(5,
          tableOutput("ancova_anova_tab")
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
    striped=TRUE,hover=TRUE,
    caption="Without groups",
    caption.placement=getOption("xtable.caption.placement","top")
  )
    
  ## Grouped by categorical variable
  output$sum_tab_wGroup<-renderTable({
    mtcars %>%
      select(!!sym(input$sel_num),!!sym(input$sel_cat),mpg) %>%
      group_by(!!sym(input$sel_cat)) %>%
      summarize(across(everything(),list(mean=mean,sd=sd)))},
    striped=TRUE,hover=TRUE,
    caption="Grouped by binary variable",
    caption.placement=getOption("xtable.caption.placement","top")
  )

  ### Output scatterplot
  ## ggplot
  output$scatter_plot <- renderPlotly({
    mtcars %>%
      ggplot(aes(label=model)) +
      geom_point(aes(x=!!sym(input$sel_num),y=mpg,color=!!sym(input$sel_cat)),size=2,alpha=0.7) +
      #expand_limits(x=c(0,NA),y=c(0,NA)) +
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
  
  
  ### Reactive expressions of models
  ## Without groups
  mod1<-reactive({
    req(input$chkGrp_regs=="all")
    mtcars %>%
      lm(paste0("mpg","~",input$sel_num) %>% as.formula(),data=.)
  })
  
  
  ## With groups separately
  # Level 0
  mod2a<-reactive({
    req(input$chkGrp_regs=="by_bi")
    mtcars %>%
      filter(!!sym(input$sel_cat)==0) %>%
      lm(paste0("mpg","~",input$sel_num) %>% as.formula(),data=.)
  })
  
  # Level 1
  mod2b<-reactive({
    req(input$chkGrp_regs=="by_bi")
    mtcars %>%
      filter(!!sym(input$sel_cat)==1) %>%
      lm(paste0("mpg","~",input$sel_num) %>% as.formula(),data=.)
  })
  
  
  ## With groups with interaction
  mod3<-reactive({
    req(input$chkGrp_ancova=="full_mod_wIntxn")
    mtcars %>%
      lm(paste0("mpg","~",input$sel_cat,"*",input$sel_num) %>% as.formula(),data=.)
  })
  
  
  ### Linear models
  ## Without groups
  output$mod1_tab_noGroup<-renderTable({
      mod1() %>%
        tidy() %>%
      select(-c(statistic,p.value))},
      striped=TRUE,hover=TRUE,
      caption="Model without groups",
      caption.placement=getOption("xtable.caption.placement","top")
  )
  
  ## With groups
  # Level 0
  output$mod2a_tab_wGroup<-renderTable({
    mod2a() %>%
      tidy() %>%
      select(-c(statistic,p.value))},
    striped=TRUE,hover=TRUE,
    caption="Binary variable = 0 (purple)",
    caption.placement=getOption("xtable.caption.placement","top")
    )
  
  # Level 1
  output$mod2b_tab_wGroup<-renderTable({
    mod2b() %>%
      tidy() %>%
      select(-c(statistic,p.value))},
    striped=TRUE,hover=TRUE,
    caption="Binary variable = 1 (yellow)",
    caption.placement=getOption("xtable.caption.placement","top")
  )
  
  
  ### ANCOVA output
  output$ancova_sum_tab<-renderTable({
    mod3() %>%
      anova() %>%
      tidy()},
    striped=TRUE,hover=TRUE,
    caption="Full model (with interaction)",
    caption.placement=getOption("xtable.caption.placement","top")
  )
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)


## NEXT
# add 'subtitles' for each section (e.g., Summary Statistics, Linear Models, ANCOVA)


## DONE
# added title tables using renderTable
# split up 'grouped data' into two separate reg models
# displayed separate reg models (by group levels) next to each other
# displayed ANCOVA output full model



## LAST COMMIT
# got functionality of smoothers to work
# improved layout
# turned scatter plot into a plotly
# created reactive model objects
# dynamically display stats of models
## changed varSelectInputs to selectInputs


# mod1<-lm(mpg~disp,data=mtcars)
# summary(mod1)[[4]][,1]
# summary(mod1)[[8]]


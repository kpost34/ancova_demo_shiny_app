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


#create model choices object
mod_choices=c("Mod1: Full, interactive"="mod1",
              "Mod2: Full, additive"="mod2",
              "Mod3: Different slopes, common intercept" = "mod3",
              "Mod4: No effect of continuous variable"="mod4",
              "Mod5: No effect of binary variable"="mod5",
              "Mod6: Null Model"="mod6")
            

#### Define UI 
ui <- fluidPage(
  
  ### CSS to make horizontal line thicker
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),

  ### Application title
  titlePanel("ANCOVA Demo App"),

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
      br(),
      radioButtons("rad_mod","Select regression line(s)",
                   choices=mod_choices,
                   selected=character(0)),
      # checkboxGroupInput("chkGrp_ancova","Run ANCOVA on",
      #                    choices=paste0("Mod",1:6),
      #                    inline=TRUE)
    ),

    #### Show tables and a plot of the data
    mainPanel(width=10,
      #summary stats in tables
      h3(strong("Summary Statistics")),
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
      h3(strong("Scatter Plot")),
      fluidRow(
        column(5,
          plotlyOutput("scatter_plot")
        ),
      #add tables below plot
      # fixedRow(
      #   column(tableOutput("mod1_tab"),
      #   tableOutput("mod2_tab"),
      #   
      # )
        #add vertical space between plot and table(s)
        # column(1),
        #model outputs
        column(5,
          tableOutput("mod_tab")
        )
          # br(),
          # splitLayout(
          #   tableOutput("mod2a_tab_wGroup"),
          #   tableOutput("mod2b_tab_wGroup")
          # )
        # )
      ),
      #add second horizontal line
      hr(),
      #ancova/anova table
      htmlOutput("ancova_subtitle"),
      fluidRow(
        column(5,
          tableOutput("ancova_fullmod_sum_tab")
        ),
        column(4,
          tableOutput("ancova_mod_sum_tab")
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
  ## ggplot (scatter plot colored by binary variable)
  output$scatter_plot <- renderPlotly({
    mtcars %>%
      ggplot(aes(label=model)) +
      geom_point(aes(x=!!sym(input$sel_num),y=mpg,color=!!sym(input$sel_cat)),size=2,alpha=0.7) +
      expand_limits(x=c(0,NA),y=c(0,NA)) +
      scale_color_viridis_d(end=0.7) +
      theme_bw() +
      theme(text=element_text(size=16)) -> p
    
      
    ## add abline using model(s) selected in checkbox group input
    #   if(!is.null(input$chkGrp_regs)){
    #     if(length(input$chkGrp_regs)==2){
    #       p1 + 
    #         geom_smooth(aes(x=!!sym(input$sel_num),y=mpg), method="lm",se=FALSE) +
    #         geom_smooth(aes(x=!!sym(input$sel_num),y=mpg,color=!!sym(input$sel_cat)),method="lm",se=FALSE) -> p2
    #     }
    #     else if(input$chkGrp_regs=="all"){
    #       p1 + 
    #         geom_smooth(aes(x=!!sym(input$sel_num),y=mpg), method="lm",se=FALSE) -> p2
    #     }
    #     else if(input$chkGrp_regs=="by_bi"){
    #       p1 + 
    #         geom_smooth(aes(x=!!sym(input$sel_num),y=mpg,color=!!sym(input$sel_cat)),method="lm",se=FALSE) -> p2
    #     }
    #   }
    # 
    # else{p1->p2}
    
    ## turn ggplot object into a plotly
    ggplotly(p) %>% 
      #put legend centered underneath plot
      layout(legend=list(
        orientation="h",xanchor="center",x=.5,y=-.3))
  })
  
  
  ### Reactive expressions of models
  ## Model 1: Full, interactive model (4 params)
  mod1<-reactive({
    req(input$rad_mod=="mod1")
    mtcars %>%
      lm(paste0("mpg","~",input$sel_cat,"*",input$sel_num) %>% 
           as.formula(),data=.)
  })
  
  
  ## Model 2: Full, additive model (3 params)
  mod2<-reactive({
    req(input$rad_mod=="mod2")
    mtcars %>%
      lm(paste0("mpg","~",input$sel_cat,"+",input$sel_num) %>% as.formula(),data=.)
  })
  
  
  ## Model 3: Common intercept and different slopes (3 params)
  mod3<-reactive({
    req(input$rad_mod=="mod3")
    mtcars %>%
      lm(paste0("mpg","~",input$sel_num,"+",input$sel_num,":",input$sel_cat) %>% as.formula(),data=.)
  })
  
  
  ## Model 4: No effect of continuous variable (2 params)
  mod4<-reactive({
    req(input$rad_mod=="mod4")
    mtcars %>%
      lm(paste0("mpg","~",input$sel_cat) %>% as.formula(),data=.)
  })
  
  
  ## Model 5: No effect of binary variable (2 params)
  mod5<-reactive({
    req(input$rad_mod=="mod5")
    mtcars %>%
      lm(paste0("mpg","~",input$sel_num) %>% as.formula(),data=.)
  })
  
  
  ## Model 6: Null model (1 param)
  mod6<-reactive({
    req(input$rad_mod=="mod6")
    mtcars %>%
      lm(paste0("mpg","~","1") %>% as.formula(),data=.)
  })
  
  
  
  
  # ## Linear regression without groups
  # mod1<-reactive({
  #   req(input$chkGrp_regs=="all")
  #   mtcars %>%
  #     lm(paste0("mpg","~",input$sel_num) %>% as.formula(),data=.)
  # })
  # 
  # 
  # ## Linear regression with groups separately
  # # Level 0
  # mod2a<-reactive({
  #   req(input$chkGrp_regs=="by_bi")
  #   mtcars %>%
  #     filter(!!sym(input$sel_cat)==0) %>%
  #     lm(paste0("mpg","~",input$sel_num) %>% as.formula(),data=.)
  # })
  # 
  # # Level 1
  # mod2b<-reactive({
  #   req(input$chkGrp_regs=="by_bi")
  #   mtcars %>%
  #     filter(!!sym(input$sel_cat)==1) %>%
  #     lm(paste0("mpg","~",input$sel_num) %>% as.formula(),data=.)
  # })
  
  
  # ## ANCOVA 
  # # With interaction
  # mod3a<-reactive({
  #   req(input$chkGrp_ancova=="full_mod_wIntxn")
  #   mtcars %>%
  #     lm(paste0("mpg","~",input$sel_cat,"*",input$sel_num) %>% as.formula(),data=.)
  # })
  # 
  # # Without interaction
  # mod3b<-reactive({
  #   req(input$chkGrp_ancova=="mod_noIntxn")
  #   mtcars %>%
  #     lm(paste0("mpg","~",input$sel_cat,"+",input$sel_num) %>% as.formula(),data=.)
  # })
  
  
  ### Linear models
  ## Display table
  # output$mod_tab<-renderTable({
  #   mod_est()
  # })
  
  
  ## Without groups
  
  
  
  
  # output$mod1_tab_noGroup<-renderTable({
  #     mod1() %>%
  #       tidy() %>%
  #     select(-c(statistic,p.value))},
  #     striped=TRUE,hover=TRUE,
  #     caption="Model without groups",
  #     caption.placement=getOption("xtable.caption.placement","top")
  # )
  
  ## With groups
  # Level 0
  # output$mod2a_tab_wGroup<-renderTable({
  #   mod2a() %>%
  #     tidy() %>%
  #     select(-c(statistic,p.value))},
  #   striped=TRUE,hover=TRUE,
  #   caption="Binary variable = 0 (purple)",
  #   caption.placement=getOption("xtable.caption.placement","top")
  #   )
  
  # # Level 1
  # output$mod2b_tab_wGroup<-renderTable({
  #   mod2b() %>%
  #     tidy() %>%
  #     select(-c(statistic,p.value))},
  #   striped=TRUE,hover=TRUE,
  #   caption="Binary variable = 1 (green)",
  #   caption.placement=getOption("xtable.caption.placement","top")
  # )
  
  
  ### ANCOVA output
  ## Subtitle
  output$ancova_subtitle<-renderUI({
    req(input$chkGrp_ancova)
    HTML("<b><h3>ANCOVA Model Output</b></h3>")
  })
  
  
  # ## Model
  # # With interaction
  # output$ancova_fullmod_sum_tab<-renderTable({
  #   mod3a() %>%
  #     anova() %>%
  #     tidy()},
  #   striped=TRUE,hover=TRUE,digits=2,
  #   caption="Full model (with interaction)",
  #   caption.placement=getOption("xtable.caption.placement","top")
  # )
  # 
  # # Without interaction
  # output$ancova_mod_sum_tab<-renderTable({
  #   mod3b() %>%
  #     anova() %>%
  #     tidy()},
  #   striped=TRUE,hover=TRUE,digits=2,
  #   caption="Model (without interaction)",
  #   caption.placement=getOption("xtable.caption.placement","top")
  # )
  
}


# Run the application 
shinyApp(ui = ui, server = server)


## NEXT
# add more summary stats


## DONE
# updated model choices (6 possibilities)
# developed 6 reactive models



## LAST COMMIT
# added subtitles (including dynamically displaying last one)
# updated color and scales of plot
# added model without interaction to UI and server




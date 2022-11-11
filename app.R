#Created by Keith Post on 10/18/22
#Shiny App demo for data that fit an ANCOVA


#load packages
pacman::p_load(shiny,here,tidyverse,janitor,plotly,broom,viridisLite)


#load data
data(mtcars)

#wrangle data
mtcars<-as_tibble(mtcars,rownames="model") %>%
  select(-c(cyl,gear,carb)) %>%
  mutate(across(c(vs,am),~as.factor(.x)))


#source in functions
source(here("backbone_and_functions","ancova_demo_app_func_01.R"))


#create model choices object
mod_choices=c("No model",
              "Mod1: Full, interactive"="mod1",
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
      radioButtons("rad_mod","Select regression model",
                   choices=mod_choices,
                   selected="No model"),
      # checkboxGroupInput("chkGrp_ancova","Run ANCOVA on",
      #                    choices=paste0("Mod",1:6),
      #                    inline=TRUE)
    ),

    #### Show tables and a plot of the data
    mainPanel(width=10,
      #summary stats in tables
      h3(strong("Summary Statistics")),
      fluidRow(
        column(5,
          tableOutput("sum_tab_noGroup")
        ),
        column(1),
        column(4,
          tableOutput("sum_tab_wGroup")
        )
      ),
      #add horizontal line
      hr(),
      #interactive plot
      h3(strong("Scatter Plot")),
      fluidRow(
        column(7,
          plotlyOutput("scatter_plot",height="500px")
        ),
        #full model output
        column(2,
          tableOutput("mod1_tab"),
          br(),
          tableOutput("mod2_tab_wGroup")
        )
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
  
  ### Output summary stats tables using selected variables
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

  
  ### Create reactive model object
  model<-reactive({
    req(input$rad_mod)
    switch(input$rad_mod,
           mod1=mtcars %>%
             lm(paste0("mpg","~",input$sel_cat,"*",input$sel_num) %>% as.formula(),data=.),
           mod2=mtcars %>%
             lm(paste0("mpg","~",input$sel_cat,"+",input$sel_num) %>% as.formula(),data=.),
           mod3=mtcars %>%
             lm(paste0("mpg","~",input$sel_num,"+",input$sel_cat,":",input$sel_num) %>% as.formula(),data=.),
           mod4=mtcars %>%
             lm(paste0("mpg","~",input$sel_cat) %>% as.formula(),data=.),
           mod5=mtcars %>%
             lm(paste0("mpg","~",input$sel_num) %>% as.formula(),data=.),
           mod6=mtcars %>%
             lm(paste0("mpg","~","1") %>% as.formula(),data=.)
    )
  })
  
  
  ### Output scatterplot
  ## ggplot (scatter plot colored by binary variable)
  output$scatter_plot <- renderPlotly({
    mtcars %>%
      ggplot(aes(label=model)) +
      geom_point(aes(x=!!sym(input$sel_num),y=mpg,color=!!sym(input$sel_cat)),size=2,alpha=0.7) +
      expand_limits(x=c(0,NA),y=c(0,NA)) +
      scale_color_viridis_d(end=0.7) +
      theme_bw() +
      theme(text=element_text(size=16),
            plot.title=element_text(size=12)) -> p1

    ## add line segments using model selected
    if(input$rad_mod %in% paste0("mod",1:6)) {
      #if model 1:4 selected
      if(input$rad_mod %in% paste0("mod",1:4)) {
        range1<-find_range(mtcars,FALSE,input$sel_num,input$sel_cat,0,model())
        range2<-find_range(mtcars,FALSE,input$sel_num,input$sel_cat,1,model())
    
        p1 +
          #specify viridis color values to match data points (from scale_color_viridis_d above)
          segment_line(range1,col=viridis(1,option="D",begin=0,end=0)) +
          segment_line(range2,col=viridis(1,option="D",begin=0.7,end=0.7)) -> p2
      }
      
      #if model 5 selected (no effect of binary variable)
      if(input$rad_mod=="mod5"){
        range<-find_range(mtcars,FALSE,num=input$sel_num,lm=model())
    
        p1 +
          #set color to blue
          segment_line(range,col="blue") -> p2
      }
      
      #if null model selected
      if(input$rad_mod=="mod6"){
        range<-find_range(mtcars,TRUE,num=input$sel_num,lm=model())
    
        p1 +
          #use default color black
          segment_line(range) -> p2
      }
    
    }

    else{p1->p2}
    
    
    
    ## turn ggplot object into a plotly
    ggplotly(p2) %>%
      #set deep bottom margin
      layout(margin=list(b=160),
             #put legend centered on right of plot
             legend=list(orientation="v",yanchor="center",x=1.02,y=.5)) -> pltly1
    
      #if...else contingent upon whether model is selected
      # if(!is.null(input$rad_mod)) {
      if(input$rad_mod %in% paste0("mod",1:6)) {
        pltly1 %>%
          #if so, then model equation added as caption
          layout(annotations=list(x=0,y=-.35,showarrow=F,xref="paper",yref="paper",
                              xanchor="left",yanchor="auto",xshift=0,yshift=0,
                              text=paste("Overall model:",get_formula(model())),
                              font=list(size=13))) -> pltly2
      }
    else{pltly1 -> pltly2}
    pltly2
    
  })
  
  

  
  
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
  ## Display table of full model
  output$mod1_tab<-renderTable({
    req(input$rad_mod %in% paste0("mod",1:6))
      model() %>%
        tidy() %>%
      select(-c(statistic,p.value))},
      striped=TRUE,hover=TRUE,
      caption="Overall model summary",
      caption.placement=getOption("xtable.caption.placement","top")
  )
  
  ## Display tables of lines by group (if applicable)
  output$mod2_tab_wGroup<-renderTable({
    req(input$rad_mod %in% paste0("mod",1:4))
    
    #run function
    pull_param(model(),input$sel_num,input$sel_cat)},
    striped=TRUE,hover=TRUE,
    caption="Regressions line by group",
    caption.placement=getOption("xtable.caption.placement","top"))

  
  
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
# add checkbox to display CI/PI bars (as geom_ribbon)
# perhaps create another dropdown box to allow user to select only points of a specfic value of am/vs (or both)
# later for ANCOVA analysis, have user choose manual or automated
# remove reg line(s) (radio button) once a variable (num or cat/bin) is changed--> return to "No model"--isolate?
# reconsider "engine" for generating fitted lines--use predict instead? this will make it much easier to display lines and will work 
  # with plotly better


## DONE
# added color as a column to regression line by group table
# added a "No model" option (that displays only points) and adjusted downstream code
# began developing backbone code to add CIs and conditionally add lines to plot


## LAST COMMIT
# created function and add app code to extract regression line information from larger models and
#display as a second table for mods 1-4





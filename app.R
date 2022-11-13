#Created by Keith Post on 10/18/22
#Shiny App demo for data that fit an ANCOVA


#load packages
pacman::p_load(shiny,here,tidyverse,janitor,plotly,broom,viridisLite,shinyjs)


#load data
data(mtcars)

#wrangle data
mtcars<-as_tibble(mtcars,rownames="car") %>%
  select(-c(cyl,gear,carb)) %>%
  mutate(across(c(vs,am),~as.factor(.x)))


#source in functions
source(here("backbone_and_functions","ancova_demo_app_func_01.R"))


#create model choices object
mod_choices=c("No model"="mod0",
              "Mod1: Full, interactive"="mod1",
              "Mod2: Full, additive"="mod2",
              "Mod3: Different slopes, common intercept" = "mod3",
              "Mod4: No effect of continuous variable"="mod4",
              "Mod5: No effect of binary variable"="mod5",
              "Mod6: Null Model"="mod6")
            

#### Define UI 
ui <- fluidPage(
  ### Activate shinyjs
  useShinyjs(),
  
  ### CSS to make horizontal line thicker
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),

  ### Application title
  titlePanel("ANCOVA Demo App"),

  #### Sidebar panel layout
  sidebarLayout(
    sidebarPanel(width=2,
      ## Numerical variable selector
      selectInput("sel_num","Select a continuous variable",
                  choices=mtcars %>%
                    select(where(is.numeric),-mpg) %>%
                    names()),
      ## Categorical (binary) variable selector
      selectInput("sel_cat","Select a binary variable",
                  choices=mtcars %>%
                    select(where(is.factor)) %>%
                    names()),
      br(),
      br(),
      br(),
      ### Radio buttons to select regression model
      radioButtons("rad_mod","Select regression model",
                   choices=mod_choices,
                   selected="mod0"),
      br(),
      ### Radio buttons to choose whether to display CIs (connected to toggle())
      radioButtons("rad_ci","Add confidence intervals to model",
                   choices=c("No","Yes"),
                   selected="No")
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
      fluidRow(
        column(7,
          h3(strong("Scatter Plot")),
          plotlyOutput("scatter_plot",height="500px")
        ),
        #full model output
        column(2,
          htmlOutput("model_subtitle"),
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

  
  ### Create reactive objects
  ## model object
  model<-reactive({
    # req(input$rad_mod %in% paste0("mod",1:6))
    switch(input$rad_mod,
           #note: if no model selected, model() is a data frame
           mod0=mtcars,
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
  
  
  ## Use model() to generate predictions and combine with selected data
  pred_dat<-reactive({
    combine_dat_fit(model(),mtcars,input$sel_num,input$sel_cat)
  })
  
  
  ## Tooltip labels
  # Point
  
  
  # Line
  
  # Band

  
  ### Update rad_ci radioButtons
  observe({
    toggle(id = "rad_ci", condition=input$rad_mod %in% paste0("mod",1:6))
  })
  
  
  ### Generate scatter plot and regression lines
  output$scatter_plot <- renderPlotly({
    #create scatter plot using combined pred-dat reactive object
    # pred_dat() %>%
    #   ggplot(aes(label=model)) +
    #   geom_point(aes(x=!!sym(input$sel_num),y=mpg,color=!!sym(input$sel_cat)),size=2,alpha=0.7) +
    #   expand_limits(x=c(0,NA),y=c(0,NA)) +
    #   scale_color_viridis_d(end=0.7) +
    #   theme_bw() +
    #   theme(text=element_text(size=16),
    #         plot.title=element_text(size=12)) -> p1
    
    
    # pred_dat() %>%
    #   ggplot(aes(label=model,x=!!sym(input$sel_num),y=mpg,color=!!sym(input$sel_cat))) +
    #   geom_point(data=~filter(.x,!!sym(input$sel_cat)==0),
    #              aes(color= paste(input$sel_cat,0,sep=" = "))) +
    #   geom_point(data=~filter(.x,!!sym(input$sel_cat)==1),
    #              aes(color= paste(input$sel_cat,1,sep=" = "))) +
    #   expand_limits(x=c(0,NA),y=c(0,NA)) +
    #   theme_bw() +
    #   theme(text=element_text(size=12),
    #         plot.title=element_text(size=10)) +
    #   scale_color_manual(values=viridis(2,end=0.7),
    #                      guide=guide_legend(title="Legend")) -> p1
    
    # point_label<-paste0(model,
    #                     "\n",input$sel_num,": ",!!sym(input$sel_num),
    #                     "\n","mpg",": ",mpg,
    #                     "\n",input$sel_cat,": ",!!sym(input$sel_cat))
    
    # pred_dat() %>%
    #   ggplot(aes(label=car,x=!!sym(input$sel_num),y=mpg,color=!!sym(input$sel_cat))) +
    #   geom_point(data=~filter(.x,!!sym(input$sel_cat)==0),
    #              aes(color= paste(input$sel_cat,0,sep=" = "),
    #                  text=paste0(car,
    #                              "\n",input$sel_num,": ",!!sym(input$sel_num),
    #                              "\n","mpg",": ",mpg,
    #                              "\n",input$sel_cat,": ",!!sym(input$sel_cat)))) +
    #   geom_point(data=~filter(.x,!!sym(input$sel_cat)==1),
    #              aes(color= paste(input$sel_cat,1,sep=" = "),
    #                  text=paste0(car,
    #                              "\n",input$sel_num,": ",!!sym(input$sel_num),
    #                              "\n","mpg",": ",mpg,
    #                              "\n",input$sel_cat,": ",!!sym(input$sel_cat)))) +
    #   expand_limits(x=c(0,NA),y=c(0,NA)) +
    #   theme_bw() +
    #   theme(text=element_text(size=12),
    #         plot.title=element_text(size=10)) +
    #   scale_color_manual(values=viridis(2,end=0.7),
    #                      guide=guide_legend(title="Legend")) -> p1
    
    
    pred_dat() %>%
      ggplot(aes(label=car,x=!!sym(input$sel_num),y=mpg,color=!!sym(input$sel_cat))) +
      geom_point(data=~filter(.x,!!sym(input$sel_cat)==0),
                 aes(color= paste(input$sel_cat,0,sep=" = "),
                     text=paste0(car,
                          "\n",input$sel_num,": ",!!sym(input$sel_num),
                          "\n","mpg",": ",mpg,
                          "\n",input$sel_cat,": ",!!sym(input$sel_cat))),
                 group=1) +
      geom_point(data=~filter(.x,!!sym(input$sel_cat)==1),
                 aes(color= paste(input$sel_cat,1,sep=" = "),
                     text=paste0(car,
                     "\n",input$sel_num,": ",!!sym(input$sel_num),
                     "\n","mpg",": ",mpg,
                     "\n",input$sel_cat,": ",!!sym(input$sel_cat))),
                    group=2) +
      expand_limits(x=c(0,NA),y=c(0,NA)) +
      theme_bw() +
      theme(text=element_text(size=12),
            plot.title=element_text(size=10)) +
      scale_color_manual(values=viridis(2,end=0.7),
                         guide=guide_legend(title="Legend")) -> p1
    
    #dynamically add regression lines
    p2<-add_reg_lines(p1,mod_num=input$rad_mod,num=input$sel_num,cat=input$sel_cat)
    
    #add plotly specifications
    ggplotly(p2,tooltip="text") %>%
      #set deep bottom margin
      layout(margin=list(b=160),
             #put legend centered on right of plot
             legend=list(orientation="v",yanchor="top",x=1.02,y=1)) -> pltly1

      #if...else contingent upon whether model is selected
      if(input$rad_mod %in% paste0("mod",1:6)) {
        pltly1 %>%
          #if so, then model equation added as caption
          layout(annotations=list(x=0,y=-.35,showarrow=F,xref="paper",yref="paper",
                              xanchor="left",yanchor="auto",xshift=0,yshift=0,
                              text=paste0("Overall model:"," \n", get_formula(model())),
                              align="left",font=list(size=13))) -> pltly2
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
  ## Display of subtitle
  output$model_subtitle<-renderUI({
    req(input$rad_mod %in% paste0("mod",1:6))
    HTML("<b><h3>Model Info</b></h3>")
  })
  
  
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
    caption="Regression lines by group",
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
# later for ANCOVA analysis, have user choose manual or automated
# replace br()s with function--see spaceship titanic
# create a tooltip text function
# set tooltip to display regression lines (and later ci bands)
# apply signif to CI bands


# 2) add input to display CI/PI bars (as geom_ribbon)--maybe a 3-choice, in-line set of radio buttons


## DONE
# updated function add_reg_lines to add separate points and lines to plotly
# changed field 'model' to 'car' to reduce confusion
# 'signifed' fit, upr, and lwr to 4 sigfigs
# corrected info displayed in tooltip for points and reg lines


## LAST COMMIT
# developed function to dynamically add regression lines to plots
# add conditional UI: updates radio button choices from "No" to c("No","Yes") if mod1-6 selected
# developed backbone code to incrementally display reg mods and ci bands





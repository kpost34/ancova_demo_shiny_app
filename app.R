#Created by Keith Post on 10/18/22
#Shiny App demo for data that fit an ANCOVA

#set options
options(show.signif.stars=FALSE)

#load packages
pacman::p_load(shiny,here,tidyverse,janitor,plotly,broom,viridisLite,shinyjs,rstatix,bslib)


#load data
data(mtcars)

#wrangle data
mtcars<-as_tibble(mtcars,rownames="car") %>%
  select(-c(cyl,gear,carb)) %>%
  mutate(across(c(vs,am),~as.factor(.x)))


#source in functions
source(here("backbone_and_functions","ancova_demo_app_func_01.R"))


#### Create Objects=================================================================================
### Create model choices object
mod_choices=c("No model"="mod0",
              "Mod1: Full, interactive"="mod1",
              "Mod2: Full, additive"="mod2",
              "Mod3: Different slopes, common intercept" = "mod3",
              "Mod4: No effect of continuous variable"="mod4",
              "Mod5: No effect of binary variable"="mod5",
              "Mod6: Null Model"="mod6")


### Create main panel tabs for ANCOVA
ancova_tabs<-tabsetPanel(
  id = "tabset_ancova",
  type = "hidden",
  #tabPanel ids need to match choices in selector
  tabPanel("tab_initial"),
  tabPanel("tab_active",
    radioButtons("rad_manANCOVA1","Remove a model term?",
                choices="No"),
    radioButtons("rad_manANCOVA2","Remove another model term?",
                choices="No"),
    radioButtons("rad_manANCOVA3","Remove a third model term?",
                choices="No")
  )
)
            

##### Define UI ====================================================================================
ui <- navbarPage("ANCOVA Demo App",
  ### Add theme
  theme=bslib::bs_theme(bootswatch="yeti"),
  #### Create first tabPanel (the app)==============================================================
  tabPanel("App",
  
    ### Activate shinyjs
    useShinyjs(),
    
    ### CSS to make horizontal line thicker
    tags$head(
      tags$style(HTML("hr {border-top: 1px solid #000000;}"))
    ),
  
    ### Application title
    #titlePanel("ANCOVA Demo App"),
  
    #### Variable selectors and model controls--------------------------------------------------------
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
        #add line breaks
        linebreaks(6),
        ### Radio buttons to select regression model
        radioButtons("rad_mod","Select regression model",
                     choices=mod_choices,
                     selected="mod0"),
        br(),
        ### Radio buttons to choose whether to display CIs (connected to toggle())
        radioButtons("rad_ci","Add confidence interval(s) to model?",
                     choices=c("No","Yes"),
                     selected="No")
      ),
  
      #### Show tables and a plot of the data---------------------------------------------------------
      mainPanel(width=10,
        #summary stats in tables
        fluidRow(
          h3(strong("Summary Statistics"))
        ),
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
        fluidRow(
          column(8,
            #add subtitle
            h3(strong("Scatter Plot")),
            #interactive plot
            plotlyOutput("scatter_plot",height="550px")
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
        hr()
      )
    ),
    
    #### ANCOVA Controls----------------------------------------------------------------------------
    sidebarLayout(
      sidebarPanel(width=2,
        #selector to switch tabs
        selectInput("sel_ancova","Would you like to run an ANCOVA?",
          choices=c("No"="tab_initial","Yes"="tab_active"),selected="tab_initial"),
        #tabset stored as an object--important to keep selectInput outside of tabsetPanel
        ancova_tabs
      ),
    
      #### ANCOVA Output----------------------------------------------------------------------------
      mainPanel(width=10,
        fluidRow(
          h3(strong("Analysis of Covariance"))
        ),
        fluidRow(
          htmlOutput("ancova_mod1_text"),
          tableOutput("ancova_mod1_tab")
        ),
        #build rest of UI using make_fluid_row_col fed through map()
        ancova_mainPanel
      )
    )
  ),
  
  #### Create second tabPanel (instructions)========================================================
  tabPanel("User guide",
    h3(strong("Background and Purpose")),
      "This Shiny app illustrates an ANCOVA using a subset of the mtcars dataset. Six models are 
      developed from two types of predictors--continuous and binary--and a dependent continuous 
      variable. The purpose of this app is to demonstrate how to visualize, model, and analyze data 
      appropriate for an ANCOVA. The following are the variables used in the Shiny app:",
    br(),
    
      h4("Variable definitions"),
        
      h5("Continuous predictors"),
        strong("disp"),": displacement (cu.in.)", br(),
        strong("hp"),": gross horsepower", br(),
        strong("drat"),": rear axle ratio", br(),
        strong("qsec"),": quarter mile time",
        linebreaks(2),
    
      h5("Binary predictors"),
        strong("vs"),": engine (0 = V-shaped, 1 = straight)", br(),
        strong("am"),": transmission (0 = automatic, 1 = manual)",
        linebreaks(2),
    
      h5("Continuous dependent variable"),
        strong("mpg"),": mile per gallon",
        linebreaks(2),
    
    h4("Model types"),
      strong("Mod1"),": full, interactive model with 4 parameters", br(),
      strong("Mod2"),": full, additive model with 3 parameters", br(),
      strong("Mod3"),": different slopes, shared intercept (3 parameters)", br(),
      strong("Mod4"),": no effect of continuous variable (2 parameters)", br(),
      strong("Mod5"),": no effect of binary variable (2 parameters)", br(),
      strong("Mod6"),": null model (1 parameter)",
    
      linebreaks(2),
    hr(),
    h3(strong("Instructions")),
    h4("Summary stats, plot, and models"),
    "Select a continuous variable and a binary variable using the drop-down menus. As these
    selections change, the tables of summary statistics, directly to the right, are dynamically
    updated. The scatter plot, which defaults to a setting without a model is updated as
    well and contains the numerical variable on the x-axis, mpg on the y-axis, and the points
    colored according to the binary (categorical) variable.",
    linebreaks(2),
    "Once you have found a pair of predictors to analyze, choose different models using the
    radio buttons underneath the `Select regression model` header. Each model selection generates 
    data points and regression line(s) on the plot, an equation of the overall model beneath the plot, 
    estimates and standard errors of the model terms (in the upper table), and, for models 1-4, estimates 
    of the model parameters of each line. Confidence intervals can be added to the model by selecting 
    'Yes' under the next header in the sidebar. Information associated with all model components 
    (i.e., data points, regression lines, CI bands) can be accessed by hovering your pointer over a 
    specific point, line, or band, and these components can be toggled on/off using the legend.",
    
    h4("ANCOVA"),
    "Proceed to the ANCOVA by selecting 'Yes' in the lower left drop-down menu. An ANCOVA of the full, 
    interactive model (called mod1 in this section and in the scatter plot) will be displayed. Model
    terms can be iteratively dropped to identify the most appropriate model based on simplicity and
    significance. Each time a model term is dropped, a new ANOVA table is displayed on the left
    side and an ANOVA comparing the two models is displayed on the right side"),
  
  #### Create third tabPanel (developer info)=======================================================
  tabPanel("Developer info",
    fluidRow(
      column(6,
        wellPanel(width="50%",
          p(h4(strong("Keith Post"))),
          p("If you would like to see the code for this Shiny app, please visit the",
           tags$a(href="https://github.com/kpost34/ancova_demo_shiny_app",
                  "Github repo"),
           "for this project."
          ),
          p(tags$a(href="https://github.com/kpost34","GitHub Profile")),
          p(tags$a(href="https://www.linkedin.com/in/keith-post","LinkedIn"))
        )
      )
    )
  )
)


##### Define server function========================================================================
server <- function(input, output, session) {
  
  #### Summary Stats Section------------------------------------------------------------------------
  ### Output summary stats tables using selected variables
  ## No groups
  output$sum_tab_noGroup<-renderTable({
    mtcars %>%
      stats_summarize(input$sel_num,"mpg")},
    striped=TRUE,hover=TRUE,
    caption="Without groups",
    caption.placement=getOption("xtable.caption.placement","top")
  )
    
  ## Grouped by categorical variable
  output$sum_tab_wGroup<-renderTable({
    mtcars %>%
      stats_summarize_grp(input$sel_num,"mpg",input$sel_cat)},
    striped=TRUE,hover=TRUE,
    caption="Grouped by binary variable",
    caption.placement=getOption("xtable.caption.placement","top")
  )

  
  #### LMs, Scatter Plot, and Model Info------------------------------------------------------------
  ### Create reactive objects
  ## model object
  model<-reactive({
    # req(input$rad_mod %in% paste0("mod",1:6))
    switch(input$rad_mod,
           #note: if no model selected, model() is a data frame
           # mod0=mtcars,
           mod0=NULL,
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
  

  
  ### Update rad_ci radioButtons
  observe({
    toggle(id = "rad_ci", condition=input$rad_mod %in% paste0("mod",1:6))
  })
  
  
  
  ### NEW APPROACH ###
  ## Use pred_dat() and functions to make a plot object (w/wo model and/or CI bands)
  scatter_mod_ci<-reactive({
    #create scatter plot using combined pred-dat reactive object
    p1<-make_scatter(pred_dat(),input$sel_num,input$sel_cat,car)
    
    #dynamically add regression lines
    p2<-add_reg_lines(p1,mod_num=input$rad_mod,num=input$sel_num,cat=input$sel_cat)
    
    #dynamically add CI bands
    p2 %>%
      {if(input$rad_ci=="Yes") add_ci_bands(.,input$rad_mod,input$sel_num,input$sel_cat) else .}
  })
  
  
  
  ### Generate scatter plot,reg lines, and CI bands from reactive plot and ggplotly()
  output$scatter_plot <- renderPlotly({
    
    #add plotly specifications
    ggplotly(scatter_mod_ci(),tooltip="text") %>%
      #set deep bottom margin
      layout(margin=list(b=160),
             #put legend centered on right of plot
             legend=list(orientation="v",yanchor="top",x=1.02,y=1)) -> pltly1
    
    #if...else contingent upon whether model is selected
    if(input$rad_mod %in% paste0("mod",1:6)) {
      pltly1 %>%
        #if so, then model equation added as caption
        layout(annotations=list(x=0,y=-.28,showarrow=F,xref="paper",yref="paper",
                                xanchor="left",yanchor="auto",xshift=0,yshift=0,
                                text=paste0("Overall model:"," \n", get_formula(model())),
                                align="left",font=list(size=13))) -> pltly2
    }
    else{pltly1 -> pltly2}
    pltly2
    
  })
  
  
  ### OLD CODE###
  # ### Generate scatter plot and regression lines
  # output$scatter_plot <- renderPlotly({
  # 
  #   #create scatter plot using combined pred-dat reactive object
  #   p1<-make_scatter(pred_dat(),input$sel_num,input$sel_cat,car)
  #   
  #   #dynamically add regression lines
  #   p2<-add_reg_lines(p1,mod_num=input$rad_mod,num=input$sel_num,cat=input$sel_cat)
  # 
  # 
  #   #dynamically add CI bands
  #   p3<-p2 %>%
  #     {if(input$rad_ci=="Yes") add_ci_bands(.,input$rad_mod,input$sel_num,input$sel_cat) else .}
  #   
  #   #add plotly specifications
  #   ggplotly(p3,tooltip="text") %>%
  #     #set deep bottom margin
  #     layout(margin=list(b=160),
  #            #put legend centered on right of plot
  #            legend=list(orientation="v",yanchor="top",x=1.02,y=1)) -> pltly1
  # 
  #     #if...else contingent upon whether model is selected
  #     if(input$rad_mod %in% paste0("mod",1:6)) {
  #       pltly1 %>%
  #         #if so, then model equation added as caption
  #         layout(annotations=list(x=0,y=-.28,showarrow=F,xref="paper",yref="paper",
  #                             xanchor="left",yanchor="auto",xshift=0,yshift=0,
  #                             text=paste0("Overall model:"," \n", get_formula(model())),
  #                             align="left",font=list(size=13))) -> pltly2
  #     }
  #     else{pltly1 -> pltly2}
  #     pltly2
  #   })
    
  
  
  ### Linear model output
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

  
  
  #### ANCOVA section--------------------------------------------------------------------------------
  ### Conditional UI
  ## Update tabset panel based on selection
  observeEvent(input$sel_ancova, {
    updateTabsetPanel(input="tabset_ancova",selected=input$sel_ancova)
  })
  
  
  ## Create reactive objects of choices
  # term_choices1
  term_choices1<-reactive({
    #cat or intxn
    c(input$sel_cat,paste0(input$sel_cat,":",input$sel_num))
  })

  # term_choices2
  term_choices2<-reactive({
    #cat
    if(input$rad_manANCOVA1==input$sel_cat){
      paste0(input$sel_cat,":",input$sel_num)
    }
    #num or intxn
    else if(input$rad_manANCOVA1==paste0(input$sel_cat,":",input$sel_num)){
      c(paste(input$sel_cat),paste(input$sel_num))
    }
  })

  # term_choices3
  term_choices3<-reactive({
    #cat or intxn
    if(input$rad_manANCOVA2 %in% c(input$sel_cat,paste0(input$sel_cat,":",input$sel_num))){
      paste(input$sel_num)
    }
    #num
    else if(input$rad_manANCOVA2==input$sel_num){
      paste(input$sel_cat)
    }
  })
  
  
  ## Update radioButtons for manual model selection
  # Remove first term
  #new options presented if 1) active tab selected, 2) sel_num, or 3) sel_cat changed
  observeEvent(list(input$sel_ancova,input$sel_num,input$sel_cat), {
    req(input$sel_ancova=="tab_active")
    updateRadioButtons(inputId="rad_manANCOVA1",choices=c("No",term_choices1()))
  })
  
  # Remove second term
  observeEvent(input$rad_manANCOVA1 %in% term_choices1(), {
    updateRadioButtons(inputId="rad_manANCOVA2",choices=c("No",term_choices2()))
  })

  # Remove third term
  observeEvent(input$rad_manANCOVA2 %in% term_choices2(), {
    updateRadioButtons(inputId="rad_manANCOVA3",choices=c("No",term_choices3()))
  })
  
  # Remove first term
  observeEvent(input$sel_ancova, {
    req(input$sel_ancova=="tab_active")
    updateRadioButtons(inputId="rad_manANCOVA1",choices=c("No",term_choices1()))
  })
  
  # Remove second term
  observeEvent(input$rad_manANCOVA1 %in% term_choices1(), {
    updateRadioButtons(inputId="rad_manANCOVA2",choices=c("No",term_choices2()))
  })
  
  # Remove third term
  observeEvent(input$rad_manANCOVA2 %in% term_choices2(), {
    updateRadioButtons(inputId="rad_manANCOVA3",choices=c("No",term_choices3()))
  })
  
  
  ## Reset model-dropping radio buttons when variable inputs changed
  observeEvent(input$sel_num, {
    reset("rad_manANCOVA1")
    reset("rad_manANCOVA2")
    reset("rad_manANCOVA3")
  })
  
  observeEvent(input$sel_cat, {
    reset("rad_manANCOVA1")
    reset("rad_manANCOVA2")
    reset("rad_manANCOVA3")
  })
  
  
  ### Model output
  ## Create reactive objects
  # Full, interactive model
  ancova_mod1<-reactive({
    # req(input$sel_ancova=="tab_active")
    mtcars %>%
      lm(paste0("mpg","~",input$sel_cat,"*",input$sel_num) %>% as.formula(),data=.)
  })
  
  
  # Second model (after removing one term)
  ancova_mod2<-reactive({
    if(input$rad_manANCOVA1==input$sel_cat){
      mtcars %>%
        lm(paste0("mpg","~",input$sel_num,"+",input$sel_cat,":",input$sel_num) %>% as.formula(),data=.)
    }
    else if(input$rad_manANCOVA1==paste0(input$sel_cat,":",input$sel_num)){
      mtcars %>%
        lm(paste0("mpg","~",input$sel_cat,"+",input$sel_num) %>% as.formula(),data=.)
    }
  })
  
  
  # Third model (after removing two terms)
  ancova_mod3<-reactive({
    if(input$rad_manANCOVA2 %in% c(input$sel_cat,paste0(input$sel_cat,":",input$sel_num))) {
      mtcars %>%
        lm(paste0("mpg","~",input$sel_num) %>% as.formula(),data=.)
    }
    else if(input$rad_manANCOVA2==input$sel_num) {
      mtcars %>%
        lm(paste0("mpg","~",input$sel_cat) %>% as.formula(),data=.)
    }
  })
  
  
  # Fourth model (after removing three terms)
  ancova_mod4<-lm(mpg~1,mtcars)
  

  
  ### Model selection tables
  ## Full, interactive model
  # ANOVA
  #table title
  output$ancova_mod1_text<-renderUI({
    req(input$sel_ancova=="tab_active")
    HTML("<h4>Mod1: Full, interactive model</h4>")
  })
  
  #table body
  output$ancova_mod1_tab<-renderTable({
    req(input$sel_ancova=="tab_active")
    ancova_mod1() %>%
      anova_test(type=1) %>%
      as_tibble() %>%
      select(Effect:p) %>%
      #applies sigfigs and converts to str
      mutate(p=signif(p,3) %>% formatC(format="g"))},
    striped=TRUE,hover=TRUE,
    caption="ANOVA table",
    caption.placement=getOption("xtable.caption.placement","top")
  )
  
  
  ## Second model (one term dropped)
  # ANOVA table
  #table title
  output$ancova_mod2_text<-renderUI({
    req(input$sel_ancova=="tab_active")
    req(input$rad_manANCOVA1 %in% term_choices1())
    if(input$rad_manANCOVA1==input$sel_cat){
      HTML("<h4>Mod3: Different slopes, common intercept</h4>")
    }
    else if(input$rad_manANCOVA1==paste0(input$sel_cat,":",input$sel_num)){
      HTML("<h4>Mod2: Full, additive model</h4>")
    }
  })
  
  #table body
  output$ancova_mod2_tab<-renderTable({
    req(input$sel_ancova=="tab_active")
    req(input$rad_manANCOVA1 %in% term_choices1())
    ancova_mod2() %>%
      anova_test(type=1) %>%
      as_tibble() %>%
      select(Effect:p) %>%
      #applies sigfigs and converts to str
      mutate(p=signif(p,3) %>% formatC(format="g"))},
    striped=TRUE,hover=TRUE,
    caption="ANOVA table",
    caption.placement=getOption("xtable.caption.placement","top")
  )
  
  # Comparison ANOVA
  output$ancova_comp1_tab<-renderTable({
    req(input$sel_ancova=="tab_active")
    req(input$rad_manANCOVA1 %in% term_choices1())
    anova(ancova_mod1(),ancova_mod2())[1:6] %>%
      rename(SS="Sum of Sq",
             p=`Pr(>F)`) %>%
      mutate(p=ifelse(!is.na(p),
                      signif(p,3) %>% formatC(format="g"),
                      NA_character_))},
    striped=TRUE,hover=TRUE,
    caption="Comparison between current and previous models",
    caption.placement=getOption("xtable.caption.placement","top")
  )
  
  
  ## Third model (one term dropped)
  # ANOVA table
  #table title
  output$ancova_mod3_text<-renderUI({
    req(input$sel_ancova=="tab_active")
    req(input$rad_manANCOVA2 %in% term_choices2())
    if(input$rad_manANCOVA2 %in% c(input$sel_cat,paste0(input$sel_cat,":",input$sel_num))) {
      HTML("<h4>Mod5: No effect of binary variable</h4>")
    }
    else if(input$rad_manANCOVA2==input$sel_num) {
      HTML("<h4>Mod4: No effect of continuous variable</h4>")
    }
  })
  
  #table body
  output$ancova_mod3_tab<-renderTable({
    req(input$sel_ancova=="tab_active")
    req(input$rad_manANCOVA2 %in% term_choices2())
    ancova_mod3() %>%
      anova_test(type=1) %>%
      as_tibble() %>%
      select(Effect:p) %>%
      #applies sigfigs and converts to str
      mutate(p=signif(p,3) %>% formatC(format="g"))},
    striped=TRUE,hover=TRUE,
    caption="ANOVA table",
    caption.placement=getOption("xtable.caption.placement","top")
  )    
  
  
  # Comparison ANOVA
  output$ancova_comp2_tab<-renderTable({
    req(input$sel_ancova=="tab_active")
    req(input$rad_manANCOVA2 %in% term_choices2())
    anova(ancova_mod2(),ancova_mod3())[1:6] %>%
      rename(SS="Sum of Sq",
             p=`Pr(>F)`) %>%
      mutate(p=ifelse(!is.na(p),
                      signif(p,3) %>% formatC(format="g"),
                      NA_character_))},
    striped=TRUE,hover=TRUE,
    caption="Comparison between current and previous models",
    caption.placement=getOption("xtable.caption.placement","top")
  )


  ## Fourth model (three terms dropped) [null model]
  # ANOVA table
  #table title
  output$ancova_mod4_text<-renderUI({
    req(input$sel_ancova=="tab_active")
    req(input$rad_manANCOVA3 %in% term_choices3())
    HTML("<h4>Mod6: Null model</h4>")})
  
  #table body
  output$ancova_mod4_tab<-renderTable({
    req(input$sel_ancova=="tab_active")
    req(input$rad_manANCOVA3 %in% term_choices3())
    ancova_mod4 %>%
      #use tidy because can't get p-val from anova() of null model
      tidy() %>%
      rename(p=p.value) %>%
      mutate(p=signif(p,3) %>% formatC(format="g"))},
    striped=TRUE,hover=TRUE,
    caption="Model summary",
    caption.placement=getOption("xtable.caption.placement","top")
  )

  # Comparison ANOVA
  output$ancova_comp3_tab<-renderTable({
    req(input$sel_ancova=="tab_active")
    req(input$rad_manANCOVA3 %in% term_choices3())
    anova(ancova_mod3(),ancova_mod4)[1:6] %>%
      rename(SS="Sum of Sq",
             p=`Pr(>F)`) %>%
      mutate(p=ifelse(!is.na(p),
                      signif(p,3) %>% formatC(format="g"),
                      NA_character_))},
    striped=TRUE,hover=TRUE,
    caption="Comparison between current and previous models",
    caption.placement=getOption("xtable.caption.placement","top")
    )
  
}



# Run the application 
shinyApp(ui = ui, server = server)





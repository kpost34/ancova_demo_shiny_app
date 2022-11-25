#Created by Keith Post on 11/6/22
#Functions for ANCOVA Demo App


#### UI Functions===================================================================================

### Function to easily create multiple line breaks--------------------------------------------------
linebreaks <- function(n){HTML(strrep(br(), n))}


### Function to create UI layout--------------------------------------------------------------------
## New function
make_fluid_row_col<-function(n){
  fluidRow(
    htmlOutput(paste0("ancova","_","mod",n,"_text")),
    column(5,
           tableOutput(paste0("ancova","_","mod",n,"_tab"))
    ),
    column(5,
           tableOutput(paste0("ancova","_","comp",n-1,"_tab"))
    )
  )
}

## New function fed through map() to build multiple sections of UI
ancova_mainPanel<-map(2:4,make_fluid_row_col)


#### Server Functions===============================================================================
### Functions to calculate summary stats------------------------------------------------------------
## Ungrouped data
stats_summarize<-function(data,num1,num2,cat){
  data %>%
    #select only numerical variable and mpg (num2)
    select(!!sym(num1),!!sym(num2)) %>% 
    #pivot to long format
    pivot_longer(cols=everything(),names_to="var",values_to="measure") %>%
    #group by variable for summary stats
    group_by(var) %>%
    #perform list of summary stats
    summarize(across(measure,list(min=min,median=median,mean=mean,max=max,sd=sd),.names="{.fn}")) %>%
    ungroup() %>%
    #turn var into factor with independent numerical variable first in order
    mutate(var=factor(var,levels=c(num1,num2))) %>%
    arrange(var)
}


## Grouped data
stats_summarize_grp<-function(data,num1,num2,cat){
  data %>%
    select(!!sym(num1),!!sym(num2),!!sym(cat)) %>% 
    pivot_longer(cols=c(!!sym(num1),!!sym(num2)),names_to="var",values_to="measure") %>%
    #group by numerical variables and cat levels
    group_by(var,!!sym(cat)) %>%
    summarize(across(measure,list(min=min,median=median,mean=mean,max=max,sd=sd),.names="{.fn}")) %>%
    ungroup() %>%
    mutate(var=factor(var,levels=c(num1,num2))) %>%
    arrange(var)
}

### Function to predict values---------------------------------------------------------------------
# find_range<-function(data,null=FALSE,num=NA,cat=NA,cat_val=NA,lm){
#   # #first four models (1:4)
#   if(null==FALSE & !is.na(num) & !is.na(cat)) {
#     #pull filter on cat var and pull x range
#     data %>%
#       filter(!!sym(cat)==cat_val) %>%
#       pull(!!sym(num)) %>%
#       range() -> xrange
# 
#     #create tbl using xrange and cat value
#     range_tbl<-tibble(!!sym(num) := xrange,
#                       !!sym(cat) := rep(cat_val,2) %>% as.factor())
# 
#     #predict yrange from param_tbl
#     yrange<-predict(lm,range_tbl) %>% unname()
#   }
# 
#   # #mod5 (no effect of binary variable)
#   if(null==FALSE & !is.na(num) & is.na(cat)) {
#     #grab xrange from DF
#     xrange<-data %>% pull(!!sym(num)) %>% range()
# 
#     #predict yrange from model and xrange
#     yrange<-predict(lm,tibble(!!sym(num):=xrange)) %>% unname()
#   }
#   
#   #mod6 (null model) if null==TRUE
#   if(null==TRUE){
#     #grab range from num and store as xrange
#     xrange<-data %>% pull(!!sym(num)) %>% range()
#     
#     #store ys as vector of mpg
#     ys<-data %>% select(mpg)
#     
#     #get range of predicted ys (same number) and store as yrange
#     yrange<-predict(lm,ys) %>% range()
#   }
#   
#   #return vector values
#   return(c("xmin"=xrange[1],"xmax"=xrange[2],"y1"=yrange[1],"y2"=yrange[2]))
# }


### Function to easily build geom_segments 
# segment_line<-function(ranges,col="black"){
#   geom_segment(x=ranges["xmin"],
#                xend=ranges["xmax"],
#                y=ranges["y1"],
#                yend=ranges["y2"],
#                color=col,
#                size=1)
# }


### Function to extract equation from lm objects----------------------------------------------------
## adapted from AlexB on Stack Overflow
get_formula <- function(model) {
  
  broom::tidy(model)[, 1:2] %>%
    mutate(sign = ifelse(sign(estimate) == 1, ' + ', ' - ')) %>% #coeff signs
    mutate_if(is.numeric, ~ abs(round(., 2))) %>% #for improving formatting
    mutate(a=case_when(
      term == "(Intercept)" & sign==" - "  ~ paste0('mpg ~ ', sign, estimate), 
      term == "(Intercept)" & sign==" + "  ~ paste0('mpg ~ ', estimate), 
      TRUE                                 ~ paste0(sign,estimate,"(",term,")"))) %>%
    summarize(formula = paste(a, collapse = '')) %>%
    as.character
}



### Function to pull out individual regression lines from regression models-------------------------
pull_param<-function(lm,slope,cat){
  #create vectors for filtering
  grp0_terms<-c("(Intercept)",slope)
  grp1_terms<-c("(Intercept)",paste0(cat,1))
  
  #extract information for group 0 model
  tidy(lm) %>%
    filter(term %in% grp0_terms) %>%
    mutate(color = "purple",
           !!sym(cat) := "0",
           parameter=str_replace(term,slope,"slope")) %>%
    select(color:parameter,estimate) -> mod_grp0
  
  #extract information for group 1 model
  tidy(lm) %>%
    mutate(parameter=ifelse(term %in% grp1_terms,
                            "(Intercept)",
                            "slope")) %>%
    group_by(parameter) %>%
    summarize(estimate=sum(estimate)) %>%
    ungroup() %>%
    mutate(color="green",
           !!sym(cat) := "1") -> mod_grp1
  
  #bind together DFs
  bind_rows(mod_grp0,mod_grp1)
}



### Function to combine model fit and CI with data--------------------------------------------------
combine_dat_fit<-function(mod,data,num,cat){
  type<-class(mod)=="lm"
  
  data %>%
    #choose num, cat, and dep vars
    select(car,!!sym(num),!!sym(cat),mpg) %>%
    #if type set to TRUE, then fit model and CI added to DF
    {if(type) bind_cols(.,predict(mod,data,interval="confidence") %>% 
                          as_tibble %>%
                          mutate(across(fit:upr,~signif(.x,4)))) else .} 
}



### Function to create scatter plot-----------------------------------------------------------------
make_scatter<-function(data,num,cat,lab){
  #make labels and vec
  lab0<-paste(cat,0,sep=" = ")
  lab1<-paste(cat,1,sep=" = ")
  
  cols<-viridis(2,end=0.7)
  names(cols)<-c(lab0,lab1)
  
  #create plot
  data %>%
    ggplot(aes(label={{lab}},x=!!sym(num),y=mpg,color=!!sym(cat))) +
    geom_point(data=~filter(.x,!!sym(cat)==0),
               aes(color= lab0,
                   text=paste0({{lab}},
                               "\n",num,": ",!!sym(num),
                               "\n","mpg",": ",mpg,
                               "\n",cat,": ",!!sym(cat))),
               #adding group was key to getting tooltip info to display properly
               group=1) +
    geom_point(data=~filter(.x,!!sym(cat)==1),
               aes(color= lab1,
                   text=paste0({{lab}},
                               "\n",num,": ",!!sym(num),
                               "\n","mpg",": ",mpg,
                               "\n",cat,": ",!!sym(cat))),
               group=2) +
    expand_limits(x=c(0,NA),y=c(0,NA)) +
    theme_bw() +
    theme(text=element_text(size=12),
          plot.title=element_text(size=10)) +
    scale_color_manual(values=cols,
                       guide=guide_legend(title="Legend")) 
}



### Function to add regression lines to plot--------------------------------------------------------
add_reg_lines<-function(plot_obj,mod_num,num,cat){
  ## create color labels and vecs
  #points only
  lab0<-paste(cat,0,sep=" = ")
  lab1<-paste(cat,1,sep=" = ")
  
  #mods 1-4
  #labels
  lab0_1_4<-paste0("model: ",cat," = 0")
  lab1_1_4<-paste0("model: ",cat," = 1")

  #vec
  cols1_4<-c(viridis(2,end=0.7),
            viridis(2,end=0.7))
  names(cols1_4)<-c(lab0,lab1,lab0_1_4,lab1_1_4)
  
  #mod 5
  cols5<-c(viridis(2,end=0.7),"blue")
  names(cols5)<-c(lab0,lab1,"model")
  
  #mod6
  cols6<-c(viridis(2,end=0.7),"black")
  names(cols6)<-c(lab0,lab1,"model")


  #if models 1-4 selected, add separate line for each cat level
  if(mod_num %in% paste0("mod",1:4)) {
    plot_obj +
      geom_line(data=~filter(.x,!!sym(cat)==0),
                aes(x=!!sym(num),y=fit,color=lab0_1_4,
                    text=paste0(
                      "\n",num,": ",!!sym(num),
                      "\n","fit",": ",fit,
                      "\n",cat,": ",!!sym(cat))),
                group=1) +
      geom_line(data=~filter(.x,!!sym(cat)==1),
                aes(x=!!sym(num),y=fit,color=lab1_1_4,
                    text=paste0(
                      "\n",num,": ",!!sym(num),
                      "\n","fit",": ",fit,
                      "\n",cat,": ",!!sym(cat))),
                group=2) +
      scale_color_manual(values=cols1_4,
                         guide=guide_legend(title="Legend"))
  }

  #if model 5 selected (no effect of binary variable), add one regression line
  else if(mod_num=="mod5"){
    plot_obj +
      geom_line(aes(x=!!sym(num),y=fit,color="model",
                    #note that cat var info removed
                    text=paste0(
                      "\n",num,": ",!!sym(num),
                      "\n","fit",": ",fit)),
                group=3) +
      scale_color_manual(values=cols5,
                         guide=guide_legend(title="Legend"))
  }

  #if null model selected, add horizontal line at mean y
  else if(mod_num=="mod6"){
    plot_obj +
      geom_line(aes(x=!!sym(num),y=fit,color="model",
                    #note that cat var info removed
                    text=paste0(
                      "\n",num,": ",!!sym(num),
                      "\n","fit",": ",fit)),
                group=3) +
      scale_color_manual(values=cols6,
                         guide=guide_legend(title="Legend"))
  }

  #if no model selected, no line is added
  else if(mod_num=="mod0"){
    plot_obj
  }
}



#### Function to add CI bands to plot---------------------------------------------------------------
add_ci_bands<-function(plot_obj,mod_num,num,cat){
  ## create color labels and vecs
  #points only
  lab0<-paste(cat,0,sep=" = ")
  lab1<-paste(cat,1,sep=" = ")
  
  #mods 1-4
  #labels
  lab0_1_4<-paste0("model: ",cat," = 0")
  lab1_1_4<-paste0("model: ",cat," = 1")
  
  lab0_ci1_4<-paste0("ci: ",cat," = 0")
  lab1_ci1_4<-paste0("ci: ",cat," = 1")
  
  #vec
  cols_ci1_4<-c(viridis(2,end=0.7),
             viridis(2,end=0.7))
  names(cols_ci1_4)<-c(lab0,lab1,lab0_1_4,lab1_1_4)
  
  #mod 5
  cols_ci5<-c(viridis(2,end=0.7),"blue","gray50")
  names(cols_ci5)<-c(lab0,lab1,"model","ci")
  
  #mod6
  cols_ci6<-c(viridis(2,end=0.7),"black","gray50")
  names(cols_ci6)<-c(lab0,lab1,"model","ci")
  
  
  #models 1-4
  if(mod_num %in% paste0("mod",1:4)) {
    plot_obj +
      #separate CIs for labeling
      geom_ribbon(data=~filter(.x,!!sym(cat)==0),
                  aes(x=!!sym(num),ymin=lwr,ymax=upr,color=lab0_ci1_4,
                      text=paste0(
                        "\n",num,": ",!!sym(num),
                        "\n","upr",": ",upr,
                        "\n","lwr",": ",lwr,
                        "\n",cat,": ",!!sym(cat))),
                  fill="gray50",alpha=0.2,group=1) +
      geom_ribbon(data=~filter(.x,!!sym(cat)==1),
                  aes(x=!!sym(num),ymin=lwr,ymax=upr,color=lab1_ci1_4,
                      text=paste0(
                        "\n",num,": ",!!sym(num),
                        "\n","upr",": ",upr,
                        "\n","lwr",": ",lwr,
                        "\n",cat,": ",!!sym(cat))),
                  fill="gray50",alpha=0.2,group=2) +
      #manually apply viridis scale
      scale_color_manual(values=cols_ci1_4,
                         guide=guide_legend(title="Legend")) 
  }
  
  #if model 5 selected (no effect of binary variable)
  else if(mod_num=="mod5"){
    plot_obj +
      #separate CIs for labeling
      geom_ribbon(aes(x=!!sym(num),ymin=lwr,ymax=upr,color="ci",
                      text=paste0(
                        "\n",num,": ",!!sym(num),
                        "\n","upr",": ",upr,
                        "\n","lwr",": ",lwr)),
                  fill="gray50",alpha=0.2,group=3) +
      #manually apply viridis scale
      scale_color_manual(values=cols_ci5,
                         guide=guide_legend(title="Legend")) 
  }
  
  #if null model selected
  else if(mod_num=="mod6"){
    plot_obj +
      #separate CIs for labeling
      geom_ribbon(aes(x=!!sym(num),ymin=lwr,ymax=upr,color="ci",
                      text=paste0(
                        "\n",num,": ",!!sym(num),
                        "\n","upr",": ",upr,
                        "\n","lwr",": ",lwr)),
                  fill="gray50",alpha=0.2,group=3) +
      #manually apply viridis scale
      scale_color_manual(values=cols_ci6,
                         guide=guide_legend(title="Legend"))
  }
  
  #if no model selected
  else if(mod_num=="mod0"){
    plot_obj 
  }
}

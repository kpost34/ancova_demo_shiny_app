#Created by Keith Post on 11/6/22
#Functions for ANCOVA Demo App

#### Server Functions===============================================================================
### Function to predict values
find_range<-function(data,null=FALSE,num=NA,cat=NA,cat_val=NA,lm){
  # #first four models (1:4)
  if(null==FALSE & !is.na(num) & !is.na(cat)) {
    #pull filter on cat var and pull x range
    data %>%
      filter(!!sym(cat)==cat_val) %>%
      pull(!!sym(num)) %>%
      range() -> xrange

    #create tbl using xrange and cat value
    range_tbl<-tibble(!!sym(num) := xrange,
                      !!sym(cat) := rep(cat_val,2) %>% as.factor())

    #predict yrange from param_tbl
    yrange<-predict(lm,range_tbl) %>% unname()
  }

  # #mod5 (no effect of binary variable)
  if(null==FALSE & !is.na(num) & is.na(cat)) {
    #grab xrange from DF
    xrange<-data %>% pull(!!sym(num)) %>% range()

    #predict yrange from model and xrange
    yrange<-predict(lm,tibble(!!sym(num):=xrange)) %>% unname()
  }
  
  #mod6 (null model) if null==TRUE
  if(null==TRUE){
    #grab range from num and store as xrange
    xrange<-data %>% pull(!!sym(num)) %>% range()
    
    #store ys as vector of mpg
    ys<-data %>% select(mpg)
    
    #get range of predicted ys (same number) and store as yrange
    yrange<-predict(lm,ys) %>% range()
  }
  
  #return vector values
  return(c("xmin"=xrange[1],"xmax"=xrange[2],"y1"=yrange[1],"y2"=yrange[2]))
}


### Function to easily build geom_segments 
segment_line<-function(ranges,col="black"){
  geom_segment(x=ranges["xmin"],
               xend=ranges["xmax"],
               y=ranges["y1"],
               yend=ranges["y2"],
               color=col,
               size=1)
}


### Function to extract equation from lm objects
## from AlexB on Stack Overflow
get_formula <- function(model) {
  
  broom::tidy(model)[, 1:2] %>%
    mutate(sign = ifelse(sign(estimate) == 1, ' + ', ' - ')) %>% #coeff signs
    mutate_if(is.numeric, ~ abs(round(., 2))) %>% #for improving formatting
    mutate(a = ifelse(term == '(Intercept)', paste0('y ~ ', estimate), paste0(sign, estimate, ' * ', term))) %>%
    summarise(formula = paste(a, collapse = '')) %>%
    as.character
  
}



### Functions to pull out individual regression lines from regression models========================
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











  



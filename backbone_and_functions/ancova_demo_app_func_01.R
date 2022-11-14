#Created by Keith Post on 11/6/22
#Functions for ANCOVA Demo App

#### Function to predict values=====================================================================
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


### Function to extract equation from lm objects
## from AlexB on Stack Overflow
get_formula <- function(model) {
  
  broom::tidy(model)[, 1:2] %>%
    mutate(sign = ifelse(sign(estimate) == 1, ' + ', ' - ')) %>% #coeff signs
    mutate_if(is.numeric, ~ abs(round(., 2))) %>% #for improving formatting
    mutate(a = ifelse(term == '(Intercept)', paste0('mpg ~ ', estimate), paste0(sign, estimate, ' * ', term))) %>%
    summarise(formula = paste(a, collapse = '')) %>%
    as.character
  
}



### Function to pull out individual regression lines from regression models========================
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



#### Function to combine model fit and CI with data=================================================
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



#### Function to add regression lines to plot=======================================================
add_reg_lines<-function(plot_obj,mod_num,num,cat){
  
  
  #if models 1-4 selected, add separate line for each cat level
  if(mod_num %in% paste0("mod",1:4)) {
    plot_obj +
      geom_line(data=~filter(.x,!!sym(cat)==0),
                aes(x=!!sym(num),y=fit,color=paste0("model: ",cat," = 0"),
                    text=paste0(
                      "\n",num,": ",!!sym(num),
                      "\n","fit",": ",fit,
                      "\n",cat,": ",!!sym(cat))),
                group=1) +
      geom_line(data=~filter(.x,!!sym(cat)==1),
                aes(x=!!sym(num),y=fit,color=paste0("model: ",cat," = 1"),
                    text=paste0(
                      "\n",num,": ",!!sym(num),
                      "\n","fit",": ",fit,
                      "\n",cat,": ",!!sym(cat))),
                group=2) +
      scale_color_manual(values=c(viridis(2,end=0.7),viridis(2,end=0.7)),
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
      scale_color_manual(values=c("blue",viridis(2,end=0.7)),
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
      scale_color_manual(values=c("black", viridis(2,end=0.7)),
                         guide=guide_legend(title="Legend")) 
  }
  
  #if no model selected, no line is added
  else if(mod_num=="mod0"){
    plot_obj 
  }
}


#### Function to add CI lines to plot===============================================================
add_ci_bands<-function(plot_obj,mod_num,num,cat){
  #models 1-4
  if(mod_num %in% paste0("mod",1:4)) {
  plot_obj +
    #separate CIs for labeling
    geom_ribbon(data=~filter(.x,!!sym(cat)==0),
                aes(x=!!sym(num),ymin=lwr,ymax=upr,color=paste0("ci: ",cat," = 0"),
                    text=paste0(
                    "\n",num,": ",!!sym(num),
                    "\n","lwr",": ",lwr,
                    "\n","upr",": ",upr,
                    "\n",cat,": ",!!sym(cat))),
                fill="gray50",alpha=0.2,group=1) +
    geom_ribbon(data=~filter(.x,!!sym(cat)==1),
                aes(x=!!sym(num),ymin=lwr,ymax=upr,color=paste0("ci: ",cat," = 1"),
                    text=paste0(
                      "\n",num,": ",!!sym(num),
                      "\n","lwr",": ",lwr,
                      "\n","upr",": ",upr,
                      "\n",cat,": ",!!sym(cat))),
                fill="gray50",alpha=0.2,group=2) +
    #manually apply viridis scale
    scale_color_manual(values=c(rep("gray50",2),viridis(2,end=0.7),viridis(2,end=0.7)),
                       guide=guide_legend(title="Legend")) 
  }
  
  #if model 5 selected (no effect of binary variable)
  else if(mod_num=="mod5"){
    plot_obj +
      #separate CIs for labeling
      geom_ribbon(aes(x=!!sym(num),ymin=lwr,ymax=upr,color="ci",
                      text=paste0(
                        "\n",num,": ",!!sym(num),
                        "\n","lwr",": ",lwr,
                        "\n","upr",": ",upr)),
                  fill="gray50",alpha=0.2,group=3) +
      #manually apply viridis scale
      scale_color_manual(values=c("gray50","blue",viridis(2,end=0.7)),
                         guide=guide_legend(title="Legend")) 
  }
    
  #if null model selected
  else if(mod_num=="mod6"){
    plot_obj +
      #separate CIs for labeling
      geom_ribbon(aes(x=!!sym(num),ymin=lwr,ymax=upr,color="ci",
                      text=paste0(
                        "\n",num,": ",!!sym(num),
                        "\n","lwr",": ",lwr,
                        "\n","upr",": ",upr)),
                  fill="gray50",alpha=0.2,group=3) +
      #manually apply viridis scale
      scale_color_manual(values=c("gray50","black", viridis(2,end=0.7)),
                         guide=guide_legend(title="Legend"))
  }
      
  #if no model selected
  else if(mod_num=="mod0"){
    plot_obj 
  }
}











  



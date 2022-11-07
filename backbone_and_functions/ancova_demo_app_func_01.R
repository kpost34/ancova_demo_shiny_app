#Created by Keith Post on 11/6/22
#Functions for ANCOVA Demo App

#### Server Functions===============================================================================
### Function to easily pull parameters from lm objects
pull_param<-function(lm,param="(Intercept)"){
  tidy(lm) %>%
    filter(term==param) %>%
    pull(estimate)
}

### Function to predict values
find_range<-function(data,num=NA,cat=NA,cat_val=NA,lm){
  
  if(is.na(num) & is.na(cat)){
    
  }
  
  else if(is.na(num) & !is.na(cat)){
    
  }
  
  else if(!is.na(num) & !is.na(cat)){
    
  }
  
  #pull filter on cat var and pull x range
  data %>%
    filter(!!sym(cat)==cat_val) %>%
    pull(!!sym(num)) %>%
    range() -> xrange
  
  #create xmin and max objects
  xmin<-xrange[1]
  xmax<-xrange[2]
  
  #create tbl using xrange and cat value
  range_tbl<-tibble(!!sym(num) := xrange,
                    !!sym(cat) := rep(cat_val,2) %>% as.factor())
  
  #predict yrange from param_tbl
  yrange<-predict(lm,range_tbl)
  
  #set ymin and ymax
  y1<-unname(yrange)[1]
  y2<-unname(yrange)[2]
  
  #compile x and y ranges into vector
  return(c("xmin"=xmin,"xmax"=xmax,"y1"=y1,"y2"=y2))
  
}


### Function to build geom_segments easily
segment_line<-function(ranges){
  geom_segment(x=ranges["xmin"],
               xend=ranges["xmax"],
               y=ranges["y1"],
               yend=ranges["y2"],
               size=1)
}













  



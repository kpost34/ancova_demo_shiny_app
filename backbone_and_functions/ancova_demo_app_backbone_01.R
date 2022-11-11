#Created by Keith Post on 11/6/22
#Backbone code for ANCOVA Demo App

mtcars<-as_tibble(mtcars)
mtcars$am<-as.factor(mtcars$am)

#### Create models==================================================================================
mod1<-lm(mpg~am*disp,data=mtcars)
mod2<-lm(mpg~am+disp,data=mtcars)
mod3<-lm(mpg~disp+am:disp,data=mtcars)
mod4<-lm(mpg~am,data=mtcars)
mod5<-lm(mpg~disp,data=mtcars)
mod6<-lm(mpg~1,data=mtcars)


#### Plot data with regression lines================================================================
### Full, interactive model (mod1)
ggplot(mtcars) + 
  geom_point(aes(x=disp,y=mpg,color=as.factor(am))) + 
  geom_smooth(method="lm",se=FALSE,
              #includes color
              aes(y=mpg,x=disp,color=as.factor(am))) +
  theme_bw()


### Simple, linear model (mod5)
ggplot(mtcars) + 
  geom_point(aes(x=disp,y=mpg,color=as.factor(am))) + 
  geom_smooth(method="lm",se=FALSE,
              #no color
              aes(y=mpg,x=disp)) +
  theme_bw()


### Null model (mod6)
ggplot(mtcars) + 
  geom_point(aes(x=disp,y=mpg,color=as.factor(am))) + 
  geom_smooth(method="lm",se=FALSE,
              #no color
              aes(y=mpg,x=disp),
              #provide null model
              formula=y~1) +
  theme_bw()

### Remaining models (mod2, mod3, and mod4)
## mod2 (as an example)
# Extract num var range for specified cat var
mtcars %>%
  filter(!!sym("am")==0) %>%
  pull(!!sym("disp")) %>%
  range() -> xrange

# Set aside xmin and xmax
xmin<-xrange[1]
xmax<-xrange[2]

# Create tbl of num and cat values
range_tbl<-tibble(!!sym("disp") :=xrange,
                  !!sym("am") :=rep(0,2))

# Use values to predict y range
yrange<-predict(mod2,param_tbl)

# Set aside y range values
y1<-unname(yrange)[1]
y2<-unname(yrange)[2]

# Return named vector of x and y values
c("xmin"=xmin,"xmax"=xmax,"y1"=y1,"y2"=y2)

# Repeat with am==1
mtcars %>%
  filter(!!sym("am")==1) %>%
  pull(!!sym("disp")) %>%
  range() -> xrange

xmin<-xrange[1]
xmax<-xrange[2]

range_tbl<-tibble(!!sym("disp") :=xrange,
                  !!sym("am") :=rep(1,2))

yrange<-predict(mod2,param_tbl)

y1<-yrange[1]
y2<-yrange[2]

c("xmin"=xmin,"xmax"=xmax,"y1"=y1,"y2"=y2)


### mod5 (no effect of binary variable)
xrange<-mtcars %>% pull(!!sym("disp")) %>% range() 

yrange<-predict(mod5,tibble(!!sym("disp"):=xrange)) %>% unname()

c("xmin"=xrange[1],"xmax"=xrange[2],"y1"=yrange[1],"y2"=yrange[2])


### Null model (mod6) 
xrange<-mtcars %>% pull(!!sym("disp")) %>% range() 

ys<-mtcars %>% select(mpg) 

predict(mod6,ys) %>% range() -> yrange

c("xmin"=xrange[1],"xmax"=xrange[2],"y1"=yrange[1],"y2"=yrange[2])



#### Pull separate models (by group) when regression model has two lines============================
group0_terms<-c("(Intercept)","disp")
group1_terms<-c("(Intercept)","am1")

### mod1
## Group 0
tidy(mod1) %>%
  filter(term %in% group0_terms) %>%
  mutate(color= "purple",
         am = "0",
         parameter=str_replace(term,"disp","slope")) %>%
  select(color:parameter,estimate) -> model1_grp0

## Group 1
tidy(mod1) %>%
  mutate(parameter=ifelse(term %in% group1_terms,
                          "(Intercept)",
                          "slope")) %>%
  group_by(parameter) %>%
  summarize(estimate=sum(estimate)) %>%
  ungroup() %>%
  mutate(color="green",
         am = "1") -> model1_grp1

## Bind together DFs
bind_rows(model1_grp0,model1_grp1)


### Find CI and PI==================================================================================
### Confidence Interval
## mod1
## Get values
predict(mod1,newdata=mtcars,interval="confidence") -> mod2_ci

## Join with data
mod2_ci %>%
  as_tibble() %>%
  bind_cols(mtcars %>%
              select(disp,am,mpg)) -> mod2_ci_df

## Plot
mod2_ci_df %>%
  ggplot() +
  geom_point(aes(x=disp,y=mpg,color=am)) +
  geom_line(data=~filter(.x,am==0),
            aes(x=disp,y=fit,color=am)) +
  geom_line(data=~filter(.x,am==1),
            aes(x=disp,y=fit,color=am)) +
  geom_ribbon(data=~filter(.x,am==0),
              aes(x=disp,ymin=lwr,ymax=upr),alpha=0.3) +
  geom_ribbon(data=~filter(.x,am==1),
              aes(x=disp,ymin=lwr,ymax=upr),alpha=0.3) +
  scale_color_viridis_d(end=0.7)

x<-2

mod2_ci_df %>%
  ggplot() +
  geom_point(aes(x=disp,y=mpg,color=am)) +
  {if(x==1) (geom_line(data=~filter(.x,am==0),aes(x=disp,y=fit,color=am)) +) else .+} 
  {if(x==2) geom_line(data=~filter(.x,am==1),aes(x=disp,y=fit,color=am)) + else .+} +
  geom_ribbon(data=~filter(.x,am==0),
              aes(x=disp,ymin=lwr,ymax=upr),alpha=0.3) +
  geom_ribbon(data=~filter(.x,am==1),
              aes(x=disp,ymin=lwr,ymax=upr),alpha=0.3) +
  scale_color_viridis_d(end=0.7)
  
            
            
            

















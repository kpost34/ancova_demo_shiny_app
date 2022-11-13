#Created by Keith Post on 11/6/22
#Backbone code for ANCOVA Demo App

mtcars<-as_tibble(mtcars,rownames="car") %>%
  select(-c(cyl,gear,carb)) %>%
  mutate(across(c(vs,am),~as.factor(.x)))

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
## Get values
# mod1
predict(mod1,newdata=mtcars,interval="confidence") -> mod1_ci

# mod5
predict(mod5,newdata=mtcars,interval="confidence") -> mod5_ci

# mod6
predict(mod6,newdata=mtcars,interval="confidence") -> mod6_ci

## Join with data
# mod1
mod1_ci %>%
  as_tibble() %>%
  bind_cols(mtcars %>%
              select(disp,am,mpg)) -> mod1_ci_df

# mod5
mod5_ci %>%
  as_tibble() %>%
  bind_cols(mtcars %>%
              select(disp,am,mpg)) -> mod5_ci_df

# mod6
mod6_ci %>%
  as_tibble() %>%
  bind_cols(mtcars %>%
              select(disp,am,mpg)) -> mod6_ci_df


## Plot
# mod1: with lines and ribbons
mod1_ci_df %>%
  #put x, y, and color in ggplot()
  ggplot(aes(x=disp,y=mpg,color=am)) +
  #separate points for labeling
  geom_point(data=~filter(.x,am==0),aes(color="am = 0")) +
  geom_point(data=~filter(.x,am==1),aes(color ="am = 1")) +
  #separate lines for labeling
  geom_line(data=~filter(.x,am==0),
            aes(x=disp,y=fit,color="model: am=0")) +
  geom_line(data=~filter(.x,am==1),
            aes(x=disp,y=fit,color="model: am=1")) +
  #separate CIs for labeling
  geom_ribbon(data=~filter(.x,am==0),
              aes(x=disp,ymin=lwr,ymax=upr,color="ci: am = 0"),
              fill="gray50",
              alpha=0.2) +
  geom_ribbon(data=~filter(.x,am==1),
              aes(x=disp,ymin=lwr,ymax=upr,color="ci: am = 1"),
              fill="gray50",
              alpha=0.2) +
  #manually apply viridis scale
  scale_color_manual(values=c(viridis(2,end=0.7),rep("gray50",2),viridis(2,end=0.7)),
                     guide=guide_legend(title="Legend")) +
  theme_bw() -> plot1 

ggplotly(plot1)

# mod1: add lines and bands separately
#start with points
mod1_ci_df %>%
  #put x, y, and color in ggplot()
  ggplot(aes(x=disp,y=mpg,color=am)) + 
  #separate points for labeling
  geom_point(data=~filter(.x,am==0),aes(color= "am = 0")) +
  geom_point(data=~filter(.x,am==1),aes(color ="am = 1")) +
  theme_bw() +
  scale_color_manual(values=viridis(2,end=0.7),
                     guide=guide_legend(title="Legend")) -> plot1a

ggplotly(plot1a)
  
  
#add lines
plot1a +
  #separate lines for labeling
  geom_line(data=~filter(.x,am==0),
            aes(x=disp,y=fit,color="model: am=0")) +
  geom_line(data=~filter(.x,am==1),
            aes(x=disp,y=fit,color="model: am=1")) +
  #manually apply viridis scale
  scale_color_manual(values=c(viridis(2,end=0.7),viridis(2,end=0.7)),
                     guide=guide_legend(title="Legend")) -> plot1b

ggplotly(plot1b)

#add CI bands
plot1b +
  #separate CIs for labeling
  geom_ribbon(data=~filter(.x,am==0),
              aes(x=disp,ymin=lwr,ymax=upr,color="ci: am = 0"),
              fill="gray50",
              alpha=0.2) +
  geom_ribbon(data=~filter(.x,am==1),
              aes(x=disp,ymin=lwr,ymax=upr,color="ci: am = 1"),
              fill="gray50",
              alpha=0.2) +
  #manually apply viridis scale
  scale_color_manual(values=c(viridis(2,end=0.7),rep("gray50",2),viridis(2,end=0.7)),
                     guide=guide_legend(title="Legend")) -> plot1c


ggplotly(plot1c)


# mod5: add lines and bands incrementally
#start with points
mod5_ci_df %>%
  #put x, y, and color in ggplot()
  ggplot(aes(x=disp,y=mpg,color=am)) + 
  #separate points for labeling
  geom_point(data=~filter(.x,am==0),aes(color= "am = 0")) +
  geom_point(data=~filter(.x,am==1),aes(color ="am = 1")) +
  theme_bw() +
  scale_color_manual(values=viridis(2,end=0.7),
                     guide=guide_legend(title="Legend")) -> plot5a

ggplotly(plot5a)


#add lines
plot5a +
  #separate lines for labeling
  geom_line(aes(x=disp,y=fit,color="model")) +
  #manually apply viridis scale
  scale_color_manual(values=c(viridis(2,end=0.7),"blue"),
                     guide=guide_legend(title="Legend")) -> plot5b

ggplotly(plot5b)

#add CI bands
plot5b +
  #separate CIs for labeling
  geom_ribbon(aes(x=disp,ymin=lwr,ymax=upr,color="ci"),
              fill="gray50",
              alpha=0.2) +
  #manually apply viridis scale
  scale_color_manual(values=c(viridis(2,end=0.7),"gray50","blue"),
                     guide=guide_legend(title="Legend")) -> plot5c

ggplotly(plot5c)



# mod6: add lines and bands incrementally
#start with points
mod6_ci_df %>%
  #put x, y, and color in ggplot()
  ggplot(aes(x=disp,y=mpg,color=am)) + 
  #separate points for labeling
  geom_point(data=~filter(.x,am==0),aes(color= "am = 0")) +
  geom_point(data=~filter(.x,am==1),aes(color ="am = 1")) +
  theme_bw() +
  scale_color_manual(values=viridis(2,end=0.7),
                     guide=guide_legend(title="Legend")) -> plot6a

ggplotly(plot6a)


#add lines
plot6a +
  #separate lines for labeling
  geom_line(aes(x=disp,y=fit,color="model")) +
  #manually apply viridis scale
  scale_color_manual(values=c(viridis(2,end=0.7),"black"),
                     guide=guide_legend(title="Legend")) -> plot6b

ggplotly(plot6b)

#add CI bands
plot6b +
  #separate CIs for labeling
  geom_ribbon(aes(x=disp,ymin=lwr,ymax=upr,color="ci"),
              fill="gray50",
              alpha=0.2) +
  #manually apply viridis scale
  scale_color_manual(values=c(viridis(2,end=0.7),"gray50","black"),
                     guide=guide_legend(title="Legend")) -> plot6c

ggplotly(plot6c)






# mod2_ci_df %>%
#   ggplot() +
#   geom_point(aes(x=disp,y=mpg,color=am)) +
#   {if(x==1) (geom_line(data=~filter(.x,am==0),aes(x=disp,y=fit,color=am)) +) else .+} 
#   {if(x==2) geom_line(data=~filter(.x,am==1),aes(x=disp,y=fit,color=am)) + else .+} +
#   geom_ribbon(data=~filter(.x,am==0),
#               aes(x=disp,ymin=lwr,ymax=upr),alpha=0.3) +
#   geom_ribbon(data=~filter(.x,am==1),
#               aes(x=disp,ymin=lwr,ymax=upr),alpha=0.3) +
#   scale_color_viridis_d(end=0.7)
  
            
            
            

















#Created by Keith Post on 11/6/22
#Backbone code for ANCOVA Demo App

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











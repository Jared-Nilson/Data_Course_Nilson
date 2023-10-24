library(tidyverse)
library(easystats)
library(janitor)
library(MASS)
library(modelr)
mush.dat <- read.csv("C:/Users/18017/Desktop/Data_Course_Nilson/Data/mushroom_growth.csv")
mush.dat
ggplot(mush.dat, aes(x=Nitrogen, y=GrowthRate, color=Light)) +
  geom_point()+
  facet_wrap(~ Species)
#cute graph but not super telling other than that nitrogen seems 
#to form a negative parabola /bell curve with the growth rate
#gonna switch light and nitrogen
ggplot(mush.dat, aes(x=Light, y=GrowthRate, color=Nitrogen)) +
  geom_point()+
  facet_wrap(~ Species)

ggplot(mush.dat, aes(x=Nitrogen, y=GrowthRate, color=Species)) +
  geom_point()+
  facet_wrap(~ Light)
#this graph is the most telling after switching light species and nitrogen
#we can see that as light increases growth rate average seems to jump up
#and the middle level of nitrogen seems to be the best at 20 or 25
mod1 <- glm(data = mush.dat, formula = GrowthRate ~ Nitrogen)
mod2 <- glm(data = mush.dat, formula = GrowthRate ~ Nitrogen + Light)
mod3 <- glm(data = mush.dat, formula = GrowthRate ~ Nitrogen + Light + Temperature + Humidity + Species)
mod4 <- glm(data = mush.dat, formula = GrowthRate ~ Nitrogen * Light * Temperature * Humidity * Species)
step <- stepAIC(mod4)
step$formula
mbest<-glm(data = mush.dat, 
           formula = step$formula)
#made 4 models and a best model
mod1 %>% summary()
# put all models into a list
mods <- list(mod1=mod1,mod2=mod2,mod3=mod3,mod4=mod4,mbest=mbest)
# apply "performance" function on all in the list and combine 
#the list of mean squared error goes from mbest to mod1 from top to bottom
map(mods,performance) %>% reduce(full_join)
compare_performance(mod1,mod2,mbest,mod3,mod4) %>% plot

predictions <-
  add_predictions(mush.dat,mbest)
predictions
predictions %>% 
  ggplot(aes(x=Nitrogen)) +
  geom_point(aes(y=GrowthRate,color="Actual")) +
  geom_point(aes(y=pred,color="Predicted"))+
  facet_wrap(~Light)


mbest$formula

nonlinear <- read_csv("C:/Users/18017/Desktop/Data_Course_Nilson/Data/non_linear_relationship.csv")
nonlinear
fit <- lm(response ~ poly(predictor, 2), data = nonlinear)
ggplot(nonlinear, aes(x=predictor, y=response)) +
  geom_point() +
  geom_line(aes(x=predictor, predict(fit)))


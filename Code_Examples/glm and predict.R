install.packages("easystats")
library(easystats)
library(tidyverse)
library(palmerpenguins)
mpg %>% names
mpg

mpg  %>% 
  ggplot(aes(x=displ,y=hwy,color=factor(cyl)))+
  geom_smooth(method='lm')

m <- glm(data = mpg,formula = hwy ~ displ + factor(cyl))
summary(m)

m$coefficients[2]

penguins%>%
  ggplot(aes(y=bill_depth_mm,x=bill_length_mm, color=species)) +
  geom_point()+
  geom_smooth(method="lm",aes(linetype=sex),se=FALSE)

#different if species isnt added
names(penguins)
#make linear regression of this plot 
#s
m1<-glm(data = penguins, formula = bill_depth_mm ~ bill_length_mm) 
  #summary()
#picks your default based on alphabetical order
m2<-glm(data = penguins, formula = bill_depth_mm ~ bill_length_mm + species) 
  #summary()
#just asks is this a good explaining variable ^^^^

#change plus to multiply star
m3<- glm(data = penguins, formula = bill_depth_mm ~ bill_length_mm * species) 
 # summary()
#adds slope for all species not just one and then adjust it

#compares models
compare_performance(m1,m2,m3,m4)
#want model to be good clean information
#what more preditors could we add
names(penguins)
m4 %>% summary()
m4 <-glm(data = penguins, formula = bill_depth_mm ~ bill_length_mm*species+sex)

formula(m4)
x<- data.frame(penguins, bill_length_mm =c(5000,100),
               species = c("chinstrap","chinstrap"),
               sex= c("male","male"),
               island = c("Dream","Dream"))
predict(m4,newdata =x)
#have to transform to log scaleo or somehing like it to be more accurate
mpg %>%
  ggplot(aes(x=displ,y=hwy))+
  geom_point() +
  geom_smooth(method="lm", formula = y ~log(x))
mpg %>% names()
y<- data.frame(displ = 40)
m5 <- glm(data=mpg,formula = hwy ~ log(displ))

#have to bring back from log scale
10^predict(m5,y)
#predict is the end goal

Titanic %>% as.data.frame()


#LOGISTIC REGRESSION MEANS OUTCOME IS TRUE FALSE, 
#all you have to do is say family = binomial in the formula
df <- read.csv("./Data/GradSchool_Admissions.csv")
df<- df%>%
  mutate(admit = as.logical(admit))
df
#outcome variable is admit which is yes or no
m6 <- glm(data = df, 
          formula = admit ~ gre +gpa +rank,
          family = 'binomial')
  summary(m6)
library(modelr)
add_predictions(df,m6,type='response')  
#column of pred are percentages
add_predictions(df,m6,type='response')  %>% 
  ggplot(aes(x=gpa,y=pred,color=factor(rank))) +
  geom_smooth()

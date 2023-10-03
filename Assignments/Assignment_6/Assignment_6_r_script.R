install.packages('gganimate')
library(ggplot2)
library(tidyverse)
library(dplyr)
library(janitor)
library(gganimate)
dat <- read_csv("BIoLOg_Plate_Data.csv") %>%
  clean_names()
dat 
class(dat$sample_id)

#Making a new column for water or soil based on the first column
dat <- dat %>%  
  mutate(new_col = if_else(dat$sample_id == "Clear_Creek", true = 'Water', false = "Soil"), 
         .after = "sample_id") %>% 
  rename("Type"="new_col") %>%
  mutate(Type = if_else(dat$sample_id == "Waste_Water", true = 'Water', false = Type))%>% 
  pivot_longer(starts_with("hr"),
               names_to = "hour",
               values_to = "absorbance", names_prefix = "hr_")%>%
  mutate_at(c('hour'), as.numeric)
print(dat, n=900)
#FIRST GRAPH
dilution_1 <- dat %>% subset(dat$dilution == .1)

dilution_1

ggplot(dilution_1, aes(x=hour, y=absorbance ,color=Type))+
  geom_smooth(se = FALSE) +
  facet_wrap(as.factor(dilution_1$substrate)) +
  theme_minimal()+
  labs(title = "Just Dilution 0.1", x= "Time", y="Absorbance")

#Animated plot for Itaconic Acid
itaconic_p <- dat %>% subset(dat$substrate == "Itaconic Acid")
it1 <- itaconic_p %>% subset(itaconic_p$rep == 1)
it2<- itaconic_p %>% subset(itaconic_p$rep == 2)
it3<- itaconic_p %>% subset(itaconic_p$rep == 3)
it1 <- it1 %>% mutate(mean_absorbance = (it1$absorbance +it2$absorbance +
                              it3$absorbance)/3)
it1
print(it1, n= 36)
itplot<-ggplot(it1, aes(x=hour, y=mean_absorbance, color=sample_id))+
  geom_smooth(method = 'lm',se=FALSE) +
  facet_wrap(it1$dilution) +
  labs(x= "Time", y="Mean_Absorbance")
itplot
itplot + transition_reveal(along = hour, range = NULL)
#I keep getting an invalid times argument and nothing online is helping not sure if it's just my computer or not

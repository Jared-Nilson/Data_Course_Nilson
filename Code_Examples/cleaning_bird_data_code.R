library(tidyverse)
library(janitor)
library(ggplot2)


clean_the_bird_data<-function(x){
df <- read.csv(x) %>% clean_names()
#seperate each section and get rid the other sections so that we can pivot longer the masses into their own colomn
mass<-
df %>% 
  select(-ends_with("n"))%>%
  select( -ends_with(c("_wing","_tail","_tarsus","_bill")))

wing<-
df %>% 
  select(-ends_with("n"))%>%
  select( -ends_with(c("f_mass","_tail","_tarsus","_bill","m_mass","unsexed_mass")))

tail<-
  df %>% 
  select(-ends_with("n"))%>%
  select( -ends_with(c("f_mass","_wing","_tarsus","_bill","m_mass","unsexed_mass")))

bill <-
  df %>% 
  select(-ends_with("n"))%>%
  select( -ends_with(c("f_mass","_tail","_tarsus","_wing","m_mass","unsexed_mass")))

tarsus<-
  df %>% 
  select(-ends_with("n"))%>%
  select( -ends_with(c("f_mass","_tail","_wing","_bill","m_mass","unsexed_mass")))


mass <- mass %>% 
  pivot_longer(c(m_mass,f_mass,unsexed_mass),
               names_to = "sex",values_to = "mass") %>%
  mutate(sex = sex %>% str_remove("_mass"))

wing <- wing %>%
  pivot_longer(c(m_wing,f_wing,unsexed_wing),
               names_to = "sex",values_to = "wing") %>%
  mutate(sex = sex %>% str_remove("_wing"))

tail<- tail %>%
  pivot_longer(c(m_tail,f_tail,unsexed_tail),
               names_to = "sex",values_to = "tail") %>%
  mutate(sex = sex %>% str_remove("_tail"))

tarsus <- tarsus %>%
  pivot_longer(c(m_tarsus,f_tarsus,unsexed_tarsus),
               names_to = "sex",values_to = "tarsus") %>%
  mutate(sex = sex %>% str_remove("_tarsus"))

bill <- bill %>%
  pivot_longer(c(m_bill,f_bill,unsexed_bill),
               names_to = "sex",values_to = "bill") %>%
  mutate(sex = sex %>% str_remove("_bill"))
  
  
  
  
  
  
full<-
  bill %>%
  full_join(mass)%>%
  full_join(tail)%>%
  full_join(tarsus)%>%
  full_join(wing)%>%
  full_join(bill)
}
clean_the_bird_data("./Data/Bird_Measurements.csv")

list.files("./Data")
myfunction<-function(x=5){return(x+3)}
#the five is set as default but can be overwritten

myfunction(x=10)



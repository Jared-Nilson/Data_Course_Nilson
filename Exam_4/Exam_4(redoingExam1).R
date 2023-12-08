#I am redoing exam 1 for my exam 4.


#I
library(tidyverse)
read_csv("cleaned_covid_data.csv")
df <- read_csv("cleaned_covid_data.csv")
print(df,n=100)
class(df)
#read into a data frame 
#II
Province_State <- df[,1]
A_states <- subset(df, grepl("^A", Province_State))
head(A_states)
#A_states 
#III
A_states
ggplot(A_states, aes(x=Last_Update, y=Deaths)) + 
  geom_point(size=.75)+
  geom_smooth(method = "loess",se=FALSE)+
  facet_wrap(~ Province_State, scales = "free") 
#plot complete 
#IV
df[,c(1,7)]
#trying to get column 1 and 7 on their own to then manipulate after
library(dplyr)
state_max_fatality_rate <- df[,c(1,7)]
state_max_fatality_rate <- state_max_fatality_rate %>%
  arrange(desc(Case_Fatality_Ratio)) %>%
  distinct(Province_State, .keep_all = TRUE) %>%
  group_by(Province_State) %>%
  summarise(Case_Fatality_Ratio = sum(Case_Fatality_Ratio))
state_max_fatality_rate <- state_max_fatality_rate %>% arrange(desc(Case_Fatality_Ratio))
colnames(state_max_fatality_rate)<-c("Province_State","Maximum_Fatality_Ratio")
state_max_fatality_rate#should be the exact data frame requested.
print(state_max_fatality_rate, n=51)
#V
ggplot(state_max_fatality_rate, aes(x= reorder(Province_State, -Maximum_Fatality_Ratio), y = Maximum_Fatality_Ratio, color= Province_State)) +
  geom_bar(stat = "identity") +
  ylim(0,6) +
  theme(axis.text.x=element_text(angle = 90))+
  theme(legend.position = "none")
# I added colors for each state :) maybe bonus points ? lol
#BONUS QUESTION
total_cumulative_deaths <- df %>%
  group_by(Last_Update) %>%
  summarise(Total_Cumulative_Deaths = sum(Deaths))

ggplot(total_cumulative_deaths, aes(x = Last_Update, y = Total_Cumulative_Deaths)) +
  geom_line(linewidth=2) +
  labs(title = "Cumulative Deaths Over Time",
       x = "Date",
       y = "Cumulative Deaths",
       color = "Province/State") +
  theme(legend.position= "none")
theme_minimal()


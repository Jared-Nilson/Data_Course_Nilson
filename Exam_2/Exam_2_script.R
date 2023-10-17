library(tidyverse)
library(janitor)
library(readxl)
library(easystats)
library(readr)
#read in the data frame and tidy it
unicef_u5mr <- read_csv("unicef-u5mr.csv")
View(unicef_u5mr)
unicef <- unicef_u5mr %>% pivot_longer(starts_with("U5MR."),
                        names_to = "year",values_to = "U5MR", 
                        names_prefix = "U5MR.") %>% ungroup()
# change year vector to numeric
unicef$year <- sapply(unicef$year, as.numeric)
map(unicef, class)
names(unicef)
unicef
#1 first plot
NILSON_Plot_1.png<-ggplot(unicef, aes(x = year,y = U5MR, z = CountryName))+
  geom_line() +
  facet_wrap(~Continent)
NILSON_Plot_1.png
#2 code for making a column with the mean U5MR and then plotting it 
view(unicef)
Mean_continent <- unicef%>%
  group_by(year, Continent) %>%
  summarize(Mean_U5MR = mean(U5MR, na.rm = TRUE))
Mean_continent
unicef_mean <- unicef %>%
  left_join(Mean_continent, by = "Continent")
unicef_mean
NILSON_Plot_2.png<-ggplot(unicef_mean, aes(x = year.y, y = Mean_U5MR, color = Continent)) +
  geom_line(size = 1.5) +
  labs(title = "Mean Death Rate Over Tim e by Continent", x = "Year", y = "Mean U5MR")
#final plot for part 2
NILSON_Plot_2.png

#part 3 creating 3 models
map(unicef, class)
year_df <- data.frame(year = unicef_mean$year.y)
year_df
unicef_mean <- cbind(unicef_mean, year_df)
unicef_mean %>% names()
#model for just year
mod1 <- glm(Mean_U5MR ~ year, data = unicef_mean)
mod1 %>% summary()
#model for year and continent
mod2 <- glm(Mean_U5MR ~ year + Continent, data = unicef_mean)
mod2 %>% summary()
#model for year, continent, and their interaction term
mod3 <- glm(Mean_U5MR ~ year + Continent + year:Continent, data = unicef_mean)
mod3 %>% summary()
#comparing the performance of the models
compare_performance(mod1,mod2,mod3)
compare_performance(mod1,mod2,mod3) %>% plot
#Model 3 is clearly the best model with an R squared value of .98 meaning that our linear regression
#accounts for 98% of possible variance in the data.

#now to plot the predictions of the models 
predictions1 <- predict(mod1, newdata = unicef_mean, type = "response")
predictions2 <- predict(mod2, newdata = unicef_mean, type = "response")
predictions3 <- predict(mod3, newdata = unicef_mean, type = "response")

unicef_mean <- cbind(unicef_mean,
                     Predictions_mod1 = predictions1,
                     Predictions_mod2 = predictions2,
                     Predictions_mod3 = predictions3)
unicef_mean
predict(mod3)
library(patchwork)
plot_mod1 <- ggplot(data = unicef_mean, aes(x = year, y = Predictions_mod1, color = Continent)) +
  geom_line(size = 1.5) +
  labs(title = "Model 1", x = "", y = "Predicted U5MR") +
  theme_minimal()+
  theme(legend.position = "none")


plot_mod2 <- ggplot(data = unicef_mean, aes(x = year, y = Predictions_mod2, color = Continent)) +
  geom_line(size = 1.5) +
  labs(title = "Model 2", x = "Year", y = "") +
  theme_minimal()+
  theme(legend.position = "none")


plot_mod3 <- ggplot(data = unicef_mean, aes(x = year, y = Predictions_mod3, color = Continent)) +
  geom_line(size=1.5) +
  labs(title = "Model 3", x = "", y = "") +
  theme_minimal()

plot_mod1 + plot_mod2 + plot_mod3

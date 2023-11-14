library(readr)
library(tidyverse)
library(janitor)
library(easystats)
library(ggplot2)


df <- read_csv("FacultySalaries_1995.csv")
names(df)
#need new column with the college rank of assoc assist
#or full and then facet wrap by tier where x is the 
#new column and y is the salaries for each one so I need to make a new salary column 
salary <- df %>%
  select(ends_with(c("Salary","ID","Name","State","Tier")))
salary <- salary %>%
  pivot_longer(c(AvgFullProfSalary,AvgAssocProfSalary,AvgAssistProfSalary),
               names_to = "Rank",values_to = "Salary")%>% 
  mutate(Rank = Rank %>% str_remove("ProfSalary"))%>%
  mutate(Rank = Rank %>% str_remove("Avg"))
salary
# now we want to join this back in with the main data frame 
df<- df %>% full_join(salary)
#need to also get rid of tier VIIB
selected_tiers <- c("I", "IIA", "IIB")

# Filter the data to include only those categories
df <- df[df$Tier %in% selected_tiers, ]

df%>%view()

#now we should have the right columns to be able to make the plot 
x_colors <- c("#FF9999", "#99FF99", "#9999FF")
ggplot(df, aes(x=Rank,y=Salary,fill=Rank)) +
  geom_boxplot()+
  facet_wrap(~Tier)+
  scale_fill_manual(values = x_colors)+
  theme_minimal()
df%>%names

model <- aov(Salary ~ State + Rank + Tier, data = df)
# Print the ANOVA results
summary(model)


df2 <- read.csv("Juniper_Oils.csv")
df2 %>% view()

df2 <- df2 %>% 
  pivot_longer(c("alpha.pinene","para.cymene","alpha.terpineol","cedr.9.ene","alpha.cedrene","beta.cedrene",
                 "cis.thujopsene","alpha.himachalene","beta.chamigrene","cuparene","compound.1","alpha.chamigrene",
                 "widdrol","cedrol","beta.acorenol","alpha.acorenol","gamma.eudesmol","beta.eudesmol","alpha.eudesmol",
                 "cedr.8.en.13.ol","cedr.8.en.15.ol","compound.2","thujopsenal"),
               names_to = "Chemical_Name", values_to = "Concentration")
view(df2)

ggplot(df2, aes(x=YearsSinceBurn, y=Concentration))+
  geom_smooth(size= 1.75)+
  theme_minimal()+
  facet_wrap(~Chemical_Name, scales = "free")


df2
library(broom)

# Fit a generalized linear model
model <- glm(formula = Concentration ~ YearsSinceBurn + Chemical_Name, data = df2, family = gaussian)

# Extract and tidy the model coefficients
tidy_results <- tidy(model)

# Filter for significant results (p-value < 0.05)
significant_results <- tidy_results[tidy_results$p.value < 0.05, ]
significant_results
significant_results <- significant_results %>% mutate(term = term %>% str_remove("Chemical_Name"))
# Print the significant results
print(significant_results)

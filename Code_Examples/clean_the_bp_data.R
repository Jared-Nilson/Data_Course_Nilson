library(tidyverse)
library(readxl)

path <- "./Data/messy_bp.xlsx"
df <- read_xlsx(path)
names(df)
df
#look at excel file to find real data is from A4 to m24
#skip first three rows
visit <- read_xlsx(path, skip = 2,n_max = 0) %>% names()

df<- read_xlsx(path, range = "A4:M24") %>% clean_names()

# df %>% pivot_longer(starts_with("bp_")) %>%
#  mutate(visit = case_when(name == "bp_8" ~ 1,
#                           name == "bp_10" ~2,
#                           name == "bp_12" ~3)) %>% 
#  pivot_longer(starts_with("hr_"),
#               names_to = "visit2",
#               values_to = "heart_rate")
#leads to dead end 
#we want to get bp and hr on their own
select(1)#first column 
select(-1)#everything except first column 
#blood pressure tidy 
bp<-
df %>% #for picking columns
  select(-starts_with("hr_")) %>%
  pivot_longer(starts_with("bp_"),values_to ="bp") %>%
  mutate(visit = case_when(name == "bp_8" ~ 1,
                           name == "bp_10" ~2,
                           name == "bp_12" ~3)) %>%
  select(-name)%>%
  separate(bp,into = c("systolic","diastolic"), convert = TRUE)
bp
hr <-
df %>% #for picking columns
  select(-starts_with("bp_")) %>%
  pivot_longer(starts_with("hr_"),values_to ="hr") %>%
  mutate(visit = case_when(name == "hr_9" ~ 1,
                           name == "hr_11" ~2,
                           name == "hr_13" ~3)) %>%
  select(-name)
hr

dat <-
full_join(bp,hr)  
dat

dat <- dat %>%
  mutate(birthdate = paste(year_birth,month_of_birth,day_birth,sep="-")%>%
  as.POSIXct())

dat <- dat %>%
  mutate(race = case_when(race == "WHITE" ~ "White",
                          race == "Caucasian" ~ "White",
                          TRUE ~ race))
dat
saveRDS(dat, "./Code_examples/cleaned_bp.rds")
#want to combine dates to scientific notation
#paste("2023","10","03", sep = "-") %>% as.POSIXct()
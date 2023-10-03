library(tidyverse)
library(readxl)
library(janitor)

df1<-read_xlsx("Data/popquiz data.xlsx")


#hrlps clean the names of columns
clean_names()

#fix names (col 1)
names(df1)[1] <- "site"
df1
dates <- janitor::excel_numeric_to_date(as.numeric(df1$site[1:3]))
class(dates)
Part1 <- lubridate::month(dates, label = TRUE, abbr=TRUE) %>%
  str_to_upper()
#pull site numbers from day of month 
Part2<- lubridate::day(dates)
#change numbers to sites in col 1

finalproduct<- paste(Part1,Part2,sep = "-")
#pasted together
df1$site[1:3] <- finalproduct
df1

df1 <- df1 %>% 
  separate(site, into = c("location","site")) %>%
  pivot_longer(starts_with("week"),
               names_to = "week",
               values_to = "rel_abund", names_prefix = "week_")
df1

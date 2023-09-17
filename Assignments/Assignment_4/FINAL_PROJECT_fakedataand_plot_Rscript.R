#Assignment 4 fake data and code and plot.
#anwers to first 3 questions in text file on canvas 
#used excel to make a cvs file of fake data
library(tidyverse)
#SO  the excel import clearly didn't work exactly and maybe the real data table could be made
#better may need help/suggestion for this 
library(readxl)
FINAL_PROJECT_FAKE_DATA <- read_excel("Assignments/Assignment_4/FINAL_PROJECT_FAKE_DATA.xlsx")
# it made new names for the columns that were combined 
FINAL_PROJECT_FAKE_DATA
fdata <- FINAL_PROJECT_FAKE_DATA
names(fdata)

fdata[1:5,1]
fdata[,2:13]
fdata[3:4,2:13]
ggplot(fdata, aes(x = Year, y= college_1_graduation_rate))+
  geom_point()
#this shows the increase in graduation rate as the years go by and we gained more online classes and 
#left the covid area. obviously the data would need to be sorted through much better 

library(rvest)
?rvest
#pulls data frames from tables and other data on a web page
#also want selector gadget
install.packages("skimr")

library(modelr)
library(easystats)
library(tidyverse)
library(data.table)
library(readr)
library(skimr)
cleaned_data_not_checked <- readRDS("C:/Users/18017/Desktop/Data_Course_Nilson/Data/cleaned_data_not_checked.RDS")
view(cleaned_data_not_checked)
dim(cleaned_data_not_checked)
names(cleaned_data_not_checked)
skimr::skim(cleaned_data_not_checked)


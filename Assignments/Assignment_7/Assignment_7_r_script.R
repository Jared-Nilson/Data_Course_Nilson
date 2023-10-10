library(tidyverse)
library(janitor)
library(dplyr)
utah = read.csv("./Utah_Religions_by_County.csv") %>% clean_names()
#cant really think of what to clean on this other than what i chnge for the graph and getting rid of county after the name
utah$county <- utah$county %>% str_replace(" County", "")
utah
names(utah)
religions = names(utah)[-c(1:4)]
# make a new colomn for religion and proportion for the plot to find out most of the correlation.
utah %>%
  pivot_longer(names_to = "Religion", values_to = "Proportion",religions) %>%
  ggplot(aes(x=Proportion,y=religious)) + geom_point() + geom_smooth(method="lm") + lims(y=c(0,1)) +
  facet_wrap(~Religion,scales = "free") + theme_bw() + theme(panel.grid = element_blank(), strip.background = element_rect(fill="Gray"))
#does population of a county correlate with an proportion of a sepcific religious group in that county.
utah_poporder <- utah %>% arrange(desc(utah$pop_2010) )
utah_poporder
utah$lds
min(utah$lds)
list(utah$lds > .5)
# only 3 countys have less than .5 proportion of LDS people as their religious practicers.
# 10, 19, and 22 which are Summit with a pop of 36324, Grand with a pop of 9225, and san juan with 14746
# THese counties seem to be almost 50/50 to religious and no religious with the majority always being LDS adn the others usually catholic, evangelical,
#or southern baptist 
# the religious population also rises with the LDS population in a very well correlated linear line as seen in the graph 
cor(utah$pop_2010, utah$non_religious)
#^didn't really show much 
# but the graph also shows us that evangelical seems to have the highest proportion when the amount of non-religious people rises(besides LDS of course which dominates
#as the majority)

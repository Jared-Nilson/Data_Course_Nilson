
install.packages("beepr")
library(modelr)
library(easystats)
library(tidyverse)
library(readr)
library(patchwork)
library(skimr)



cleaned_data_not_checked <- readRDS("C:/Users/18017/Desktop/Data_Course_Nilson/Data/cleaned_data_not_checked.RDS")

names(cleaned_data_not_checked)

column_classes <- sapply(cleaned_data_not_checked, class)
print(column_classes)


#possible questions to look for answers. 




# how many lab hours does each major have shown in scatter plot

majors_filtered <- cleaned_data_not_checked %>% filter(!is.na(CURRENT_MAJOR_NAME))

#^^filtering out the NA so that the plot doesn't have an NA column


OVERALL_LABHOURS<-
  ggplot(data= majors_filtered, aes(x = CURRENT_MAJOR_NAME, y = LAB_CONTACT_HOURS, fill = CURRENT_MAJOR_NAME)) +
  geom_bar(stat = "sum",show.legend = FALSE) +
  labs(title = "Lab Hours Needed by Major", x = "Major", y = "Total Lab Hours",fill=NULL)

major_counts <- majors_filtered %>% group_by(CURRENT_MAJOR_NAME) %>% summarise(Count = n_distinct(STUDENT_WAREHOUSE_ENTITY_UID))
#how many people took each major over the whole time period
print(major_counts)

CUMULITIVE_LABHOURS<-
  ggplot(data= majors_filtered, aes(x = CURRENT_MAJOR_NAME, y = LAB_CONTACT_HOURS, fill = CURRENT_MAJOR_NAME)) +
  geom_bar(stat = "identity",show.legend = FALSE) +
  labs(title = "Cumulitive Lab Hours by Major", x = "Major", y = "Total Lab Hours",fill=NULL)

(CUMULITIVE_LABHOURS | OVERALL_LABHOURS)





# how many terms it took them to graduate seperated by no act to 10-15 16-23 24-36 score  

df2<-cleaned_data_not_checked[, c("STUDENT_WAREHOUSE_ENTITY_UID","ACT_COMPOSITE","FIRST_YEAR","ASSOC_GRAD_YEAR","BACH_GRAD_YEAR","LAST_TERM_YEAR","LENGTH_OF_ENROLLMENT")]
df2$YEARS_TO_BACH <- df2$BACH_GRAD_YEAR - df2$FIRST_YEAR
df2$YEARS_TO_ASSOC <- df2$ASSOC_GRAD_YEAR - df2$FIRST_YEAR
df2

df2$ACT_Range <- cut(df2$ACT_COMPOSITE,
                     breaks = c(-Inf, 0, 10, 15, 20, 24, 36, Inf),
                     labels = c("No_ACT", "0-10", "11-15", "16-20", "21-24", "25-36", "No_ACT"),
                     include.lowest = TRUE)
df2
# Convert NAs to "NO_ACT"
df2$ACT_Range <- fct_na_value_to_level(df2$ACT_Range, "NO_ACT")
# Create a new data frame with average values
summary_df <- aggregate(cbind(YEARS_TO_ASSOC, YEARS_TO_BACH) ~ ACT_Range, data = df2, FUN = mean)

# Reshape data for plotting
plot_data <- reshape2::melt(summary_df, id.vars = "ACT_Range")

# Plot grouped bar chart
ggplot(plot_data, aes(x = ACT_Range, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Graduation Lengths by ACT Composite Range",
       x = "ACT Composite Range",
       y = "Average Years") +
  scale_fill_manual(values = c("#89CFF0","#7851A9"), name = "Graduation Type") +
  theme_minimal()

#MODEL IF SIGNIFICANT act affect on grad time to assoc and then bach

assocmod <- glm(data=df2, formula = YEARS_TO_ASSOC ~ ACT_Range)
summary(assocmod)
bachmod <- glm(data=df2, formula = YEARS_TO_BACH ~ ACT_Range)
summary(bachmod)


# credits earned or passed linked to year OR TERM for each student in a scatter in hopes to show plateau of when people leave for a while 
#have to use previous filtered dataset so we can sort by majors
df3 <-majors_filtered[, c("STUDENT_WAREHOUSE_ENTITY_UID","ACADEMIC_PERIOD","TERM_TOTAL_CREDITS","CREDITS_PASSED","CREDITS_ATTEMPTED","CURRENT_MAJOR_NAME","GENDER_DESC")]%>%print(n=200)

#Need to add term total credits as the the acedemic period increases for each student

# Convert ACADEMIC_PERIOD to a factor for proper ordering
df3$ACADEMIC_PERIOD <- factor(df3$ACADEMIC_PERIOD, levels = c("201320","201330","201340","201420","201430","201440","201520","201530","201540","201620","201630","201640",
                                                                          "201720","201730","201740","201820","201830","201840","201920","201930","201940","202020","202030","202040",
                                                                          "202120","202130"))

# Sort the data by ACADEMIC_PERIOD within each student
df3 <- df3[order( df3$ACADEMIC_PERIOD, df3$STUDENT_WAREHOUSE_ENTITY_UID), ]

# Calculate cumulative sum of TERM_TOTAL_CREDITS for each student
df3$Cumulative_Credits <- ave(df3$CREDITS_PASSED, df3$STUDENT_WAREHOUSE_ENTITY_UID, FUN = cumsum)

# Theres too many students fro the plot to generate so I want to radnomly select 500  students instead of all of them.
#225
set.seed(231)  # Set seed for reproducibility
sample_students <- sample(unique(df3$STUDENT_WAREHOUSE_ENTITY_UID), 500)
df_sample <- df3[df3$STUDENT_WAREHOUSE_ENTITY_UID %in% sample_students, ]

df_sample
# Create the plot

major_palette <- c("Biology" = "#23925A", "Biotechnology" = "#970A0A", "Biology Education" = "#256CD3", "Botany" = "#2F11AC")

# Plotting
ggplot(df_sample, aes(x = ACADEMIC_PERIOD, y = Cumulative_Credits, group = STUDENT_WAREHOUSE_ENTITY_UID)) +
  geom_line(aes(color = CURRENT_MAJOR_NAME), lwd = 1.5, alpha = 0.55) +
  scale_color_manual(values = major_palette) +  # Assign colors based on major
  facet_grid(GENDER_DESC ~ .) +  # Facet by gender
  labs(x = "Academic Period", y = "Cumulative Term Total Credits", title = "Cumulative Term Total Credits Over Time") +
  theme(
    legend.position = "right",  # Position the legend
    strip.text.x = element_text(size = 12, face = "bold"),
    strip.background = element_rect(fill = "lightblue"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  )



#numerical better 


#have to take sample because model too large

df <- cleaned_data_not_checked[c("DEGREE","ACT_COMPOSITE","FREEZE_OF_RECORD","FREEZE_TER","CURRENT_MAJOR_NAME","GENDER_DESC","PRIM_ETHNICITY_CATEGORY_DESC","BACH_GRAD_TERM","ASSOC_GRAD_TERM","TERM_TOTAL_CREDITS","FIRST_YEAR","LENGTH_OF_ENROLLMENT")]

df
#had to check levels to see whhich ones didnt have enough levels because I kept getting an error
library(MASS)

#overall model that predicts whether they'll graduate based off everything the model can think of using quasibinomial

mod_simple_subset <- glm(DEGREE ~ ACT_COMPOSITE + CURRENT_MAJOR_NAME + FREEZE_OF_RECORD + GENDER_DESC + PRIM_ETHNICITY_CATEGORY_DESC+ LENGTH_OF_ENROLLMENT,
                         data = df,
                         family = "binomial")

mod_complicated_subset <- glm(DEGREE ~ ACT_COMPOSITE * CURRENT_MAJOR_NAME * FREEZE_OF_RECORD + GENDER_DESC * PRIM_ETHNICITY_CATEGORY_DESC* LENGTH_OF_ENROLLMENT,
                         data = df,
                         family = "binomial")
#saveRDS(mod_complicated_subset, "./bigmodel.RDS")
#beepr::beep(sound=8)

compare_performance(mod_simple_subset,mod_complicated_subset)

#made to check levels of the data set and print them out 
#df_subset.asfactor <- df_subset %>%
#  mutate_at(vars("LENGTH_OF_ENROLLMENT","ACT_COMPOSITE","FREEZE_OF_RECORD","FREEZE_TER","CURRENT_MAJOR_NAME","GENDER_DESC","PRIM_ETHNICITY_CATEGORY_DESC","BACH_GRAD_TERM","ASSOC_GRAD_TERM","TERM_TOTAL_CREDITS","FIRST_YEAR"),
 #           as.factor)
#
#for (predictor in names(df_subset.asfactor)[categorical_predictors]) {
 # cat("Predictor:", predictor, "\n")
  #cat("Levels:", unique(df_subset.asfactor[[predictor]]), "\n\n")
#}


df_subset<-add_predictions(df,mod_complicated_subset,type='response') 

df_subset[c("pred")]%>%print(n=400)#column of pred are percentages

df_subset %>%
  filter(!is.na(GENDER_DESC)) %>% 
  ggplot(aes(x=ACT_COMPOSITE,y=pred,color=factor(CURRENT_MAJOR_NAME))) +
  geom_smooth(size = 2)+
  facet_grid(GENDER_DESC ~ .)+
  labs(x = "ACT_RANGE", y = "Predicted Likliehood to get Degree", title = NULL,) +
  theme(
    legend.position = "right",  # Position the legend
    strip.text.x = element_text(size = 12, face = "bold"),
    strip.background = element_rect(fill = "lightblue"))
  


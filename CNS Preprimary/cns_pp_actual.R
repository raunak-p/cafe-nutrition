
library("rmarkdown")
library(tidyverse)
library(Hmisc)


pre_primary <- read.csv("pp_data_oct13.csv", stringsAsFactors = FALSE)


#CREATING CLEAN DATAFRAME

#column names

#REMEMBER TO ADD VEGETARIAN OPTION HERE
names_preprimary <- c("timestamp", "first_name", "last_name", "gender", "age", "grade",
                      "parent_name", "parent_email", "parent_phone", "sick", "new_food", "allergy", "dairy_milk", "dairy_curd", "dairy_cheese",
                      "dairy_paneer", "vegetables", "fruits",  "whole_grains", "sugary_foods", "fast_food", "protein_fish", 
                      "protein_chicken", "protein_meat", "protein_eggs", "protein_dal", "protein_nuts", "water", "excercise",
                      "screen", "priority", "more_often", "disappointed", "comment")

preprimary_clean <- pre_primary

colnames(preprimary_clean) <- names_preprimary

#POINTS

#Adding extra columns 
preprimary_clean <- mutate(preprimary_clean,
                           dairy_milk_p = dairy_milk,
                           dairy_curd_p = dairy_curd,
                           dairy_cheese_p = dairy_cheese,
                           dairy_paneer_p = dairy_paneer,
                           vegetables_p = vegetables,
                           fruits_p = fruits,
                           whole_grains_p = whole_grains,
                           sugary_foods_p = sugary_foods,
                           fast_food_p = fast_food,
                           protein_fish_p = protein_fish,
                           protein_chicken_p = protein_chicken,
                           protein_meat_p = protein_meat,
                           protein_eggs_p = protein_eggs,
                           protein_dal_p = protein_dal,
                           protein_nuts_p = protein_nuts)
                    


#changing text to numbers



preprimary_clean$dairy_milk_p[preprimary_clean$dairy_milk_p == "Never"] <- 0
preprimary_clean$dairy_milk_p[preprimary_clean$dairy_milk_p == "1-2 days a week"] <- 1
preprimary_clean$dairy_milk_p[preprimary_clean$dairy_milk_p == "3-4 days a week"] <- 2
preprimary_clean$dairy_milk_p[preprimary_clean$dairy_milk_p == "5-6 days a week"] <- 3
preprimary_clean$dairy_milk_p[preprimary_clean$dairy_milk_p == "All Days"] <- 4

preprimary_clean$dairy_curd_p[preprimary_clean$dairy_curd_p == "Never"] <- 0
preprimary_clean$dairy_curd_p[preprimary_clean$dairy_curd_p == "1-2 days a week"] <- 1
preprimary_clean$dairy_curd_p[preprimary_clean$dairy_curd_p == "3-4 days a week"] <- 2
preprimary_clean$dairy_curd_p[preprimary_clean$dairy_curd_p == "5-6 days a week"] <- 3
preprimary_clean$dairy_curd_p[preprimary_clean$dairy_curd_p == "All Days"] <- 4

preprimary_clean$dairy_cheese_p[preprimary_clean$dairy_cheese_p == "Never"] <- 0
preprimary_clean$dairy_cheese_p[preprimary_clean$dairy_cheese_p == "1-2 days a week"] <- 1
preprimary_clean$dairy_cheese_p[preprimary_clean$dairy_cheese_p == "3-4 days a week"] <- 2
preprimary_clean$dairy_cheese_p[preprimary_clean$dairy_cheese_p == "5-6 days a week"] <- 3
preprimary_clean$dairy_cheese_p[preprimary_clean$dairy_cheese_p == "All Days"] <- 4

preprimary_clean$dairy_paneer_p[preprimary_clean$dairy_paneer_p == "Never"] <- 0
preprimary_clean$dairy_paneer_p[preprimary_clean$dairy_paneer_p == "1-2 days a week"] <- 1
preprimary_clean$dairy_paneer_p[preprimary_clean$dairy_paneer_p == "3-4 days a week"] <- 2
preprimary_clean$dairy_paneer_p[preprimary_clean$dairy_paneer_p == "5-6 days a week"] <- 3
preprimary_clean$dairy_paneer_p[preprimary_clean$dairy_paneer_p == "All Days"] <- 4


preprimary_clean$vegetables_p[preprimary_clean$vegetables_p == "Never"] <- 0
preprimary_clean$vegetables_p[preprimary_clean$vegetables_p == "1-2 days a week"] <- 1
preprimary_clean$vegetables_p[preprimary_clean$vegetables_p == "3-4 days a week"] <- 2
preprimary_clean$vegetables_p[preprimary_clean$vegetables_p == "5-6 days a week"] <- 3
preprimary_clean$vegetables_p[preprimary_clean$vegetables_p == "All Days"] <- 4


preprimary_clean$fruits_p[preprimary_clean$fruits_p == "Never"] <- 0
preprimary_clean$fruits_p[preprimary_clean$fruits_p == "1-2 days a week"] <- 1
preprimary_clean$fruits_p[preprimary_clean$fruits_p == "3-4 days a week"] <- 2
preprimary_clean$fruits_p[preprimary_clean$fruits_p == "5-6 days a week"] <- 3
preprimary_clean$fruits_p[preprimary_clean$fruits_p == "All Days"] <- 4

preprimary_clean$whole_grains_p[preprimary_clean$whole_grains_p == "Never"] <- 0
preprimary_clean$whole_grains_p[preprimary_clean$whole_grains_p == "1-2 days a week"] <- 1
preprimary_clean$whole_grains_p[preprimary_clean$whole_grains_p == "3-4 days a week"] <- 2
preprimary_clean$whole_grains_p[preprimary_clean$whole_grains_p == "5-6 days a week"] <- 3
preprimary_clean$whole_grains_p[preprimary_clean$whole_grains_p == "All Days"] <- 4

preprimary_clean$sugary_foods_p[preprimary_clean$sugary_foods_p == "Never"] <- 4
preprimary_clean$sugary_foods_p[preprimary_clean$sugary_foods_p == "1-2 days a week"] <- 4
preprimary_clean$sugary_foods_p[preprimary_clean$sugary_foods_p == "3-4 days a week"] <- 2
preprimary_clean$sugary_foods_p[preprimary_clean$sugary_foods_p == "5-6 days a week"] <- 1
preprimary_clean$sugary_foods_p[preprimary_clean$sugary_foods_p == "All Days"] <- 0


preprimary_clean$fast_food_p[preprimary_clean$fast_food_p == "Never"] <- 4
preprimary_clean$fast_food_p[preprimary_clean$fast_food_p == "1-2 days a week"] <- 4
preprimary_clean$fast_food_p[preprimary_clean$fast_food_p == "3-4 days a week"] <- 2
preprimary_clean$fast_food_p[preprimary_clean$fast_food_p == "5-6 days a week"] <- 1
preprimary_clean$fast_food_p[preprimary_clean$fast_food_p == "All Days"] <- 0

preprimary_clean$protein_fish_p[preprimary_clean$protein_fish_p == "Never"] <- 0
preprimary_clean$protein_fish_p[preprimary_clean$protein_fish_p == "1-2 days a week"] <- 1
preprimary_clean$protein_fish_p[preprimary_clean$protein_fish_p == "3-4 days a week"] <- 2
preprimary_clean$protein_fish_p[preprimary_clean$protein_fish_p == "5-6 days a week"] <- 3
preprimary_clean$protein_fish_p[preprimary_clean$protein_fish_p == "All Days"] <- 4


preprimary_clean$protein_chicken_p[preprimary_clean$protein_chicken_p == "Never"] <- 0
preprimary_clean$protein_chicken_p[preprimary_clean$protein_chicken_p == "1-2 days a week"] <- 1
preprimary_clean$protein_chicken_p[preprimary_clean$protein_chicken_p == "3-4 days a week"] <- 2
preprimary_clean$protein_chicken_p[preprimary_clean$protein_chicken_p == "5-6 days a week"] <- 3
preprimary_clean$protein_chicken_p[preprimary_clean$protein_chicken_p == "All Days"] <- 4

preprimary_clean$protein_meat_p[preprimary_clean$protein_meat_p == "Never"] <- 0
preprimary_clean$protein_meat_p[preprimary_clean$protein_meat_p == "1-2 days a week"] <- 1
preprimary_clean$protein_meat_p[preprimary_clean$protein_meat_p == "3-4 days a week"] <- 2
preprimary_clean$protein_meat_p[preprimary_clean$protein_meat_p == "5-6 days a week"] <- 3
preprimary_clean$protein_meat_p[preprimary_clean$protein_meat_p == "All Days"] <- 4

preprimary_clean$protein_eggs_p[preprimary_clean$protein_eggs_p == "Never"] <- 0
preprimary_clean$protein_eggs_p[preprimary_clean$protein_eggs_p == "1-2 days a week"] <- 1
preprimary_clean$protein_eggs_p[preprimary_clean$protein_eggs_p == "3-4 days a week"] <- 2
preprimary_clean$protein_eggs_p[preprimary_clean$protein_eggs_p == "5-6 days a week"] <- 3
preprimary_clean$protein_eggs_p[preprimary_clean$protein_eggs_p == "All Days"] <- 4

preprimary_clean$protein_dal_p[preprimary_clean$protein_dal_p == "Never"] <- 0
preprimary_clean$protein_dal_p[preprimary_clean$protein_dal_p == "1-2 days a week"] <- 1
preprimary_clean$protein_dal_p[preprimary_clean$protein_dal_p == "3-4 days a week"] <- 2
preprimary_clean$protein_dal_p[preprimary_clean$protein_dal_p == "5-6 days a week"] <- 3
preprimary_clean$protein_dal_p[preprimary_clean$protein_dal_p == "All Days"] <- 4

preprimary_clean$protein_nuts_p[preprimary_clean$protein_nuts_p == "Never"] <- 0
preprimary_clean$protein_nuts_p[preprimary_clean$protein_nuts_p == "1-2 days a week"] <- 1
preprimary_clean$protein_nuts_p[preprimary_clean$protein_nuts_p == "3-4 days a week"] <- 2
preprimary_clean$protein_nuts_p[preprimary_clean$protein_nuts_p == "5-6 days a week"] <- 3
preprimary_clean$protein_nuts_p[preprimary_clean$protein_nuts_p == "All Days"] <- 4



preprimary_clean <- preprimary_clean %>%
  mutate(water_p = ifelse(water == "Rarely", 2,
                          ifelse(water == "Mostly", 4, NA))) %>%
  mutate(excercise_p = ifelse(excercise == "None", 0,
                              ifelse(excercise == "15 minute", 1,
                                     ifelse(excercise == "30 minutes", 2, 4))))%>%
  mutate(screen_p = case_when(.$screen == "15-60 minutes" ~ 4,
                              .$screen == "Never" ~ 4,
                              .$screen == "1-2 hours" ~ 3,
                              .$screen == "2-3 hours" ~ 2,
                              .$screen == "3+ hours" ~ 1))%>%
  mutate(his_and_her = case_when(.$gender == "Male" ~ "his",
                                 .$gender == "Female" ~ "her"))


preprimary_clean[35:52] <- apply(preprimary_clean[35:52], 2, function(x) as.numeric(x))

##USe MUTATE + CASE_WHEN NEXT TIME YOU MUTT


preprimary_clean <- preprimary_clean %>%
  mutate(dairy_total_p = dairy_milk_p + dairy_cheese_p + dairy_paneer_p +dairy_curd_p)%>%
  mutate(dairy_variety_p = (dairy_milk_p > 0) + (dairy_cheese_p  > 0) + (dairy_paneer_p  > 0) + (dairy_curd_p  > 0 ))%>%
  mutate(protein_total_p = protein_chicken_p +protein_dal_p + protein_eggs_p + protein_fish_p + protein_meat_p + protein_nuts_p)%>%
  mutate(dairy_total_p = case_when(.$dairy_total_p >= 7 ~ 10,
                                   .$dairy_total_p < 7 ~ .$dairy_total_p))%>%
  mutate(protein_total_p = case_when(.$protein_total_p >= 7 ~ 10,
                                      .$protein_total_p < 7 ~ .$protein_total_p))%>%
  mutate(healthy_p = sugary_foods_p + fast_food_p)
  



preprimary_clean <- mutate(preprimary_clean, total_p = vegetables_p + fruits_p + protein_total_p + dairy_total_p + dairy_variety_p +
                             screen_p + water_p + excercise_p + sugary_foods_p)
preprimary_clean <- mutate(preprimary_clean, first_name_c = capitalize(tolower(first_name)))
preprimary_clean  <- mutate(preprimary_clean, full_name = paste(first_name_c, last_name, sep = "_"))



for (name in preprimary_clean$full_name){
  subgroup <- preprimary_clean[preprimary_clean$full_name == name,]
  render(input = "cns_pp_actual_report.rmd",output_file = paste0(name, "_",  'report', '.pdf'))
}


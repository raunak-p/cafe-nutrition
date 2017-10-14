library(packrat)

#Loading in neccessary libraries
library(rmarkdown)
library(tidyverse)
library(Hmisc)
library(devtools)

#reading data in
primary <- read.csv("primary_data_oct13.csv", stringsAsFactors = FALSE)

## Making Clean DataFrame

#column names
names_primary <- c("timestamp", "first_name", "last_aname", "gender", "age", "grade", "division",
                      "parent_name", "parent_email", "vegetarian_or_not", "sick", "allergy", "dairy", 
                      "milk_type",  "vegetables", "potato", "fruits", "juice",  "whole_grains", "sugary_foods_week", 
                      "fast_food_week", "meat", "egg", "dal", "nuts", "water", "excercise", "screen", "dinner_with_family","priority", 
                      "more_often", "disappointed", "comment")

c_primary <- primary

colnames(c_primary) <- names_primary

## Adding Columns 

#gender pronouns

c_primary <- mutate(c_primary,
                       his_her = case_when(gender == "Male" ~ "his",
                                             gender == "Female" ~ "her"),
                       he_she = case_when(gender == "Male" ~ "he",
                                          gender == "Female" ~ "she")
                       
)

#protein: servings to grams per day 

c_primary <- mutate(c_primary,
                        dairy_prot = case_when(dairy == "Less than 1 serving" ~ 3,
                                                  #There are two spaces after "than" here. There was a TYPO
                                                  dairy == "1 serving" ~ 6,
                                                  dairy == "2 servings" ~ 12,
                                                  dairy == "3 servings" ~ 3,
                                                  dairy == "4 or more servings" ~ 24),
                        
                        veg_prot = case_when(dal == "Never" ~ 0,
                                             dal == "1/2 serving" ~ 2.5,
                                             dal == "1 serving" ~ 5,
                                             dal == "2 servings" ~ 10,
                                             dal == "3 servings or more" ~ 15),
                        
                        meat_prot = case_when(meat == "Never" ~ 0,
                                         meat == "1-2 servings" ~ 1.9,
                                         meat == "3-4 servings" ~ 4.5,
                                         meat == "5-6 servings" ~ 7,
                                         meat == "7 servings or more" ~ 9),
                        
                        grain_prot = case_when(whole_grains == "Never" ~ 0,
                                               whole_grains == "3-4 servings" ~ 7,
                                               whole_grains == "5-6 servings" ~ 12,
                                               whole_grains == "7-9 servings" ~ 17),
                        
                        egg_prot = case_when(egg == "Never" ~ 0,
                                             egg == "1-2 servings" ~1.5,
                                             egg == "3-5 servings" ~ 4,
                                             egg == "6-8 servings" ~ 7,
                                             egg == "More than 9 servings" ~ 9),
                        
                        nut_prot = case_when(nuts == "None" ~ 0,
                                             nuts == "1-3 servings" ~1.9,
                                             nuts == "4-6 servings" ~ 2.5,
                                             nuts == "7 servings or more" ~ 3.5),
                        
                        total_prot = dairy_prot + veg_prot + meat_prot + grain_prot + egg_prot + nut_prot
                        
)

#points for everything 

c_primary <- mutate(c_primary,
                    prot_p = case_when(total_prot > 22 & total_prot <= 28 ~ 10,
                                       #The number of protein points is a function of how far away you are from the ideal protein requireent
                                       TRUE ~ round((10 - scale(abs(27 - total_prot), 4, 1.5)), digits = 0)),
                    
                    prot_b = case_when(total_prot <= 22 ~ "low_prot",
                                       total_prot > 28 ~ "high_prot",
                                       TRUE ~ "normal_prot"),
                    
                    dairy_p = case_when(dairy == "None" ~ 0,
                                        dairy == "Less than 1 serving" ~ 1,
                                        #There are two spaces after "than" here. There was a TYPO
                                        dairy == "1 serving" ~ 3,
                                        dairy == "2 servings" ~ 5,
                                        dairy == "3 servings" ~ 3,
                                        dairy == "4 or more servings" ~ 1),
                    
                    dairy_b = case_when(dairy == "None" | dairy == "Less than  1 serving" |
                                          dairy == "1 serving" ~ "low_dairy",
                                        dairy == "2 servings" ~ "norm_dairy",
                                        TRUE ~ "high_dairy"),
                    
                    fruit_p = case_when(fruits == "None" ~ 0,
                                        fruits == "1 whole fruit or 1 cup fruit" ~ 3,
                                        TRUE ~ 5),
                    
                    veg_p = case_when(vegetables == "None" ~ 0,
                                      vegetables == "1 serving" ~ 3,
                                      TRUE ~ 5),
                    
                    potato_p = case_when(potato == "more than 1 cup" ~ 1,
                                         potato == "1 cup" ~ 3,
                                         TRUE ~ 5),
                    
                    sweet_p = case_when(sugary_foods_week == "Never" ~ 5,
                                        sugary_foods_week == "1-2 times" ~ 5,
                                        sugary_foods_week == "3-4 times" ~ 3,
                                        sugary_foods_week == "More than 4 times" ~ 1
                                        ),
                    
                    fast_p = case_when(fast_food_week == "Never" ~ 5,
                                       fast_food_week == "1-2 times" ~ 5,
                                       fast_food_week == "3-4 times" ~ 3,
                                       fast_food_week == "More than 4 times" ~ 1
                                       ),
                    
                    junk_p = sweet_p + fast_p,
                    
                    exercise_p = case_when(excercise == "None" | excercise ==  "15 mins or less" ~ 0,
                                           excercise == "30 mins" ~ 3,
                                           TRUE ~ 5),
                    
                    water_p = case_when(water == "Mostly" ~ 5,
                                        TRUE ~ 2),
                    
                    total_p = water_p + exercise_p + prot_p + dairy_p + veg_p + fruit_p + potato_p + junk_p
                    
                    )

#change names

c_primary <- mutate(c_primary, 
                    first_name_c = capitalize(tolower(first_name)),
                    last_name_c = capitalize(tolower(last_aname)),
                    full_name = paste(first_name_c, last_name_c, sep = "_")
)


for (name in c_primary$full_name){
  subgroup <- c_primary[c_primary$full_name == name,]
  render(input = "cns_p_report.rmd",output_file = paste0(name, "_",  'report', '.pdf'))
}





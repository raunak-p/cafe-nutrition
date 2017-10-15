#Loading in neccessary libraries
library(rmarkdown)
#library(tidyverse)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(devtools)
library(latexpdf)

#reading data
secondary <- read.csv("cns_secondary_data.csv", stringsAsFactors = FALSE)

#adding column names
names_secondary <- c("timestamp", "first_name", "last_name", "gender", "age", "grade", "division",
                   "parent_name", "parent_email", "veg_or_not", "get_sick", "any_allergy", "dairy_products", 
                   "milk_type",  "vegetables", "potato", "fruits", "juice",  "whole_grains", "sugary_foods_week", 
                   "fast_food_week", "meat", "egg", "legumes", "nuts", "water", "excercise", "screen_time",
                   "dinner_with_family","eat_stress","skip_breakfast","food_label","nutrition_topic",
                   "interact_with_cf","services_stop", "comment")

c_secondary <- secondary

colnames(c_secondary) <- names_secondary

## Adding Columns 

#gender pronouns

c_secondary <- mutate(c_secondary,
                    his_her = case_when(gender == "Male" ~ "his",
                                        gender == "Female" ~ "her"),
                    he_she = case_when(gender == "Male" ~ "he",
                                       gender == "Female" ~ "she")
                    
)

#protein: servings to grams per day 

c_secondary <- mutate(c_secondary,
                    dairy_prot = case_when(dairy_products == "Less than  1 serving" ~ 3,
                                           #There are two spaces after "than" here. There was a TYPO
                                           dairy_products == "1 serving" ~ 6,
                                           dairy_products == "2 servings" ~ 12,
                                           dairy_products == "3 servings" ~ 3,
                                           dairy_products == "4 servings or more" ~ 24),
                    
                    
                    veg_prot = case_when(legumes == "Never" ~ 0,
                                         legumes == "1/2 serving" ~ 2.5,
                                         legumes == "1 serving" ~ 5,
                                         legumes == "2 servings" ~ 10,
                                         legumes == "3 servings or more" ~ 15),
                    
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

c_secondary <- mutate(c_secondary,
                    prot_s = case_when(total_prot > 22 & total_prot <= 28 ~ 10,
                                       #The number of protein points is a function of how far away you are from the ideal protein requireent
                                       TRUE ~ round((10 - scale(abs(27 - total_prot), 4, 1.5)), digits = 0)),
                    
                    prot_b = case_when(total_prot <= 22 ~ "low_prot",
                                       total_prot > 28 ~ "high_prot",
                                       TRUE ~ "normal_prot"),
                    
                    dairy_s = case_when(dairy_products == "None" ~ 0,
                                        dairy_products == "Less than  1 serving" ~ 1,
                                        #There are two spaces after "than" here. There was a TYPO
                                        dairy_products == "1 serving" ~ 3,
                                        dairy_products == "2 servings" ~ 5,
                                        dairy_products == "3 servings" ~ 3,
                                        dairy_products == "4 servings or more" ~ 1),
                    
                    dairy_b = case_when(dairy_products == "None" | dairy_products == "Less than  1 serving" |
                                          dairy_products == "1 serving" ~ "low_dairy",
                                        dairy_products == "2 servings" ~ "norm_dairy",
                                        TRUE ~ "high_dairy"),
                    
                    fruit_s = case_when(fruits == "None" ~ 0,
                                        fruits == "1 whole fruit or 1 cup fruit" ~ 3,
                                        TRUE ~ 5),
                    
                    veg_s = case_when(vegetables == "None" ~ 0,
                                      vegetables == "1 serving" ~ 3,
                                      TRUE ~ 5),
                    
                    potato_s = case_when(potato == "more than 1 cup" ~ 1,
                                         potato == "1 cup" ~ 3,
                                         TRUE ~ 5),
                    
                    sweet_s = case_when(sugary_foods_week == "Never" ~ 5,
                                        sugary_foods_week == "1-2 times" ~ 5,
                                        sugary_foods_week == "3-4 times" ~ 3,
                                        sugary_foods_week == "More than 4 times" ~ 1
                    ),
                    
                    fast_s = case_when(fast_food_week == "Never" ~ 5,
                                       fast_food_week == "1-2 times" ~ 5,
                                       fast_food_week == "3-4 times" ~ 3,
                                       fast_food_week == "More than 4 times" ~ 1
                    ),
                    
                    junk_s = sweet_s + fast_s,
                    
                    exercise_s = case_when(excercise == "None" | excercise ==  "15 mins or less" ~ 0,
                                           excercise == "30 mins" ~ 3,
                                           TRUE ~ 5),
                    
                    water_s = case_when(water == "Mostly" ~ 5,
                                        TRUE ~ 2),
                    
                    total_s = water_s + exercise_s + prot_s + dairy_s + veg_s + fruit_s + potato_s + junk_s
                    
)

#change names

#for middle name i tried man for 2 hours but no luck, maybe you give it a shot

#c_secondary$proper_first_name = strsplit(c_secondary$first_name, " ")
#c_secondary$proper_first_name = tolower(c_secondary$proper_first_name)
#c_secondary$proper_first_name = paste()capitalize(c_secondary$proper_first_name)                                     

#simpleCap <- function(x) {
#  s <- strsplit(x, " ")[[1]]
#  paste(toupper(substring(s, 1,1)), substring(s, 2),
#        sep="", collapse=" ")
#}

c_secondary <- mutate(c_secondary,
                      proper_first_name = capitalize(tolower(first_name)),
                      proper_last_name = capitalize(tolower(last_name)),        
                      full_name = paste(proper_first_name, proper_last_name, sep = "_")

)

#make pdf name reports
subgroup <- c_secondary

for (name in c_secondary$full_name){
  subgroup <- c_secondary[c_secondary$full_name == name,]
  render(input = "sec_report.rmd",output_file = paste0(name, "_",  'report', '.pdf'))
}
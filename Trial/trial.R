#Loading Packages
library(ggplot2)
library(dplyr)
library(Hmisc)
library(rmarkdown)
library(latex2exp)
library(latexpdf)
library(pander)
library(pandocfilters)
library(knitr)
library(gridExtra)
library(stringr)

#Reading CSV data File
#Add the CSV file | Mind the data that you are using
source_data <- read.csv("data.csv", stringsAsFactors = FALSE)

#adding column names
column_names <- c(
  "timestamp", "first_name", "last_name", "gender", "age", "grade", "division",
  "parent_name", "parent_email", "veg_or_not", "get_sick", "any_allergy", "dairy_products", 
  "milk_type", "vegetables", "potato", "fruits", "juice",  "whole_grains",
  "sugary_foods_week", "fast_food_week", "meat", "egg", "legumes", "nuts",
  "water", "exercise", "screen_time","dinner_with_family","eat_stress","skip_breakfast",
  "food_label","nutrition_topic","interact_with_cf","services_stop", "comment"
)
colnames(source_data) <- column_names

#Modifying Name
source_data$first_name <- str_to_title(tolower(source_data$first_name))
source_data$last_name <- str_to_title(tolower(source_data$last_name))

#Calculating Protein Points
source_data <- mutate(source_data,
  dairy_protein = case_when(
    dairy_products == "None" ~ 0,
    dairy_products == "Less than  1 serving" ~ 3, #There are two spaces after "than" here. There was a TYPO
    dairy_products == "1 serving" ~ 6,
    dairy_products == "2 servings" ~ 12,
    dairy_products == "3 servings" ~ 3,
    dairy_products == "4 servings or more" ~ 24
    ),
                    
  veg_protein = case_when(
    legumes == "Never" ~ 0,
    legumes == "1/2 serving" ~ 2.5,
    legumes == "1 serving" ~ 5,
    legumes == "2 servings" ~ 10,
    legumes == "3 servings or more" ~ 15
    ),
                    
  meat_protein = case_when(
    meat == "Never" ~ 0,
    meat == "1-2 servings" ~ 1.9,
    meat == "3-4 servings" ~ 4.5,
    meat == "5-6 servings" ~ 7,
    meat == "7 servings or more" ~ 9
    ),
                    
  grain_protein = case_when(
    whole_grains == "Never" ~ 0,
    whole_grains == "3-4 servings" ~ 7,
    whole_grains == "5-6 servings" ~ 12,
    whole_grains == "7-9 servings" ~ 17
    ),
                  
  egg_protein = case_when(
    egg == "Never" ~ 0,
    egg == "1-2 servings" ~1.5,
    egg == "3-5 servings" ~ 4,
    egg == "6-8 servings" ~ 7,
    egg == "More than 9 servings" ~ 9
    ),
                    
  nut_protein = case_when(
    nuts == "None" ~ 0,
    nuts == "1-3 servings" ~1.9,
    nuts == "4-6 servings" ~ 2.5,
    nuts == "7 servings or more" ~ 3.5
    ),
  total_protein = dairy_protein + veg_protein + meat_protein + grain_protein + egg_protein + nut_protein
                    
)

#Ideal Protein
source_data <- mutate(source_data,
  ideal_protein_grade = case_when( 
    grade==8 |grade==9 ~53,
    grade==6 |grade==7 ~40
    )
)

#Points for Everything
source_data <-mutate(source_data,
  fruit_points = case_when(
    fruits == "None" ~ 0,
    fruits == "1 whole fruit or 1 cup fruit" ~ 3,
    TRUE ~ 5
    ),
  
  vegetable_points = case_when(
    vegetables == "None" ~ 0,
    vegetables == "1 serving" |vegetables == "2 servings" ~ 3,
    TRUE ~ 5
    ),
  
  potato_points = case_when(
    potato == "more than 1 cup" ~ 1,
    potato == "1 cup" ~ 3,
    TRUE ~ 5
    ),
  
  sugary_food_points = case_when(
    sugary_foods_week == "Never" ~ 5,
    sugary_foods_week == "1-2 times" ~ 5,
    sugary_foods_week == "3-4 times" ~ 3,
    sugary_foods_week == "More than 4 times" ~ 1
    ),
  fast_food_points = case_when(
    fast_food_week == "Never" ~ 5,
    fast_food_week == "1-2 times" ~ 5,
    fast_food_week == "3-4 times" ~ 3,
    fast_food_week == "More than 4 times" ~ 1
    ),
                     
  junk_food_points = sugary_food_points + fast_food_points,
                     
  exercise_points = case_when(
    exercise == "None" | exercise ==  "15 mins or less" ~ 0,
    exercise == "30 mins" ~ 3,
    exercise == "45 mins" | exercise == "1 hour or more" ~ 5
    ),
  
  water_points = case_when(
    water == "Mostly" ~ 5,
    water == "Rarely" ~ 2
    )
)

#Protein and Dairy                     
source_data <- mutate(source_data,
  protein_points = case_when(
    total_protein == ideal_protein_grade ~ 10,
    total_protein > ideal_protein_grade ~ round((((2*ideal_protein_grade)-total_protein)/ideal_protein_grade)*10, digits = 0),
    total_protein < ideal_protein_grade ~ round((total_protein/ideal_protein_grade)*10, digits = 0)
    ),
  protein_level = case_when(
    total_protein < ideal_protein_grade ~ "low_prot",
    total_protein > ideal_protein_grade ~ "high_prot",
    TRUE ~ "normal_prot"
    ),
  dairy_points = case_when(
    dairy_products == "None" ~ 0,
    dairy_products == "Less than  1 serving" ~ 1, #There are two spaces after "than" here. There was a TYPO
    dairy_products == "1 serving" ~ 3,
    dairy_products == "2 servings" ~ 5,
    dairy_products == "3 servings" ~ 3,
    dairy_products == "4 servings or more" ~ 1
    ),
  dairy_level = case_when(
    dairy_products == "None" | dairy_products == "Less than  1 serving" |
    dairy_products == "1 serving" ~ "low_dairy",
    dairy_products == "2 servings" ~ "norm_dairy",
    TRUE ~ "high_dairy"
    ),
  
  name=paste(first_name,last_name,sep="_"),
     
  total_points = water_points + exercise_points + protein_points + dairy_points + vegetable_points + fruit_points + potato_points + junk_food_points
) 

subgroup <- source_data[1,]
render(input = "trialrmd.Rmd", output_file = paste0(1, "_", subgroup$name, "_", "Report" , ".pdf"))

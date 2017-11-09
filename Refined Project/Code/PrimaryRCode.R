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
#Add the file
source_data <- read.csv("../Data/JNIS_Primary.csv", stringsAsFactors = FALSE)

#Adding Column Names
column_names <- c(
  "timestamp", "first_name", "last_name", "gender", "age", "grade", "division",
  "parent_name", "parent_email", "vegetarian_or_not", "sick", "allergy", "dairy", 
  "milk_type",  "vegetables", "potato", "fruits", "juice",  "whole_grains",
  "sugary_foods", "fast_food",
  "meat", "egg", "legumes", "nuts", "water", "exercise", "screen",
  "dinner_with_family","priority", "more_often", "disappointed", "comment"
  )

colnames(source_data) <- column_names

#Attaching Gender Pronouns
source_data <- mutate(source_data,
  his_her = case_when(
    gender == "Male" ~ "his",
    gender == "Female" ~ "her"
    ),
  he_she = case_when(
    gender == "Male" ~ "he",
    gender == "Female" ~ "she"
    )
)

#Modifying Name
source_data$first_name <- str_to_title(tolower(source_data$first_name))
source_data$last_name <- capitalize(tolower(source_data$last_name))

#Calculating Protein Points
source_data <- mutate(source_data,
  dairy_protein = case_when(
    dairy == "None" ~ 0,
    dairy == "Less than  1 serving" ~ 3, #There are two spaces after "than" here. There was a TYPO
    dairy == "1 serving" ~ 6,
    dairy == "2 servings" ~ 12,
    dairy == "3 servings" ~ 3,
    dairy == "4 servings or more" ~ 24
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

#Points for Everything
source_data <- mutate(source_data,
  protein_points = case_when(
    total_protein > 22 & total_protein <= 28 ~ 10,
    TRUE ~ round((10 - scale(abs(27 - total_protein), 4, 1.5)), digits = 0) #The number of protein points is a function of how far away you are from the ideal protein requirement
    ),

  protein_level = case_when(
    total_protein <= 22 ~ "low_prot",
    total_protein > 28 ~ "high_prot",
    TRUE ~ "normal_prot"
    ),

  dairy_points = case_when(
    dairy == "None" ~ 0,
    dairy == "Less than  1 serving" ~ 1, #There are two spaces after "than" here. There was a TYPO
    dairy == "1 serving" ~ 3,
    dairy == "2 servings" ~ 5,
    dairy == "3 servings" ~ 3,
    dairy == "4 or more servings" ~ 1
    ),

  dairy_level = case_when(
    dairy == "None" | dairy == "Less than  1 serving" |dairy == "1 serving" ~ "low_dairy",
    dairy == "2 servings" ~ "norm_dairy",
    TRUE ~ "high_dairy"
    ),

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
    sugary_foods == "Never" ~ 5,
    sugary_foods == "1-2 times" ~ 5,
    sugary_foods == "3-4 times" ~ 3,
    sugary_foods == "More than 4 times" ~ 1
    ),
                     
  fast_food_points = case_when(
    fast_food == "Never" ~ 5,
    fast_food == "1-2 times" ~ 5,
    fast_food == "3-4 times" ~ 3,
    fast_food == "More than 4 times" ~ 1
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
    ),

  total_points = water_points + exercise_points + protein_points + dairy_points + vegetable_points + fruit_points + potato_points + junk_food_points
)

#Generating reports
for (iterator in  1:nrow(source_data)){
  subgroup <- source_data[iterator,]
  render(input = "PrimaryRMD.Rmd", output_file = paste0("../Reports/", iterator, "_", subgroup$first_name, "_", subgroup$last_name, " ", "Report" , ".pdf"))
}
#Loading Packages
library(ggplot2)
library(dplyr)
library(Hmisc)
library(knitr)
library(rmarkdown)
library(latexpdf)
library(pander)
library(gridExtra)
library(tidyverse)

#Reading CSV data File
#Add the CSV file | Mind the data that you are using
source_data <- read.csv("../Data/_.csv", stringsAsFactors = FALSE)

#Adding names
column_names <- c(
  "timestamp", "first_name", "last_name", "gender", "age", "grade",
  "parent_name", "parent_email", "veg_nonveg",
  "sick", "new_food", "allergy",
  "dairy_milk", "dairy_curd", "dairy_cheese","dairy_paneer",
  "vegetables", "fruits",  "protein_whole_grains",
  "sugary_food", "fast_food",
  "protein_fish", "protein_chicken", "protein_meat", "protein_eggs", "protein_dal", "protein_nuts",
  "water", "exercise",
  "screen", "topics", "interact_with_cf", "services_stop_disappointment", "comment"
)
colnames(source_data) <- column_names

#Attaching Gender Pronouns
source_data <- mutate(cleaned_source_data,
  his_her = case_when(
    gender == "Male" ~ "his",
    gender == "Female" ~ "her"
    ),
  he_she = case_when(
    gender == "Male" ~ "he",
    gender == "Female" ~ "she"
    )
)

#Modifying First Name
source_data$first_name <- capitalize(tolower(first_name))

#Calculating Points
source_data <- mutate(source_data,
  dairy_milk_points = case_when(
    dairy_milk == "Never" ~ 0,
    dairy_milk == "1-2 days a week" ~ 1,
    dairy_milk == "3-4 days a week" ~ 2,
    dairy_milk == "5-6 days a week" ~ 3,
    dairy_milk == "All Days" ~ 4
    ),
  dairy_curd_points = case_when(
    dairy_curd == "Never" ~ 0,
    dairy_curd == "1-2 days a week" ~ 1,
    dairy_curd == "3-4 days a week" ~ 2,
    dairy_curd == "5-6 days a week" ~ 3,
    dairy_curd == "All Days" ~ 4
    ),
  dairy_cheese_points = case_when(
    dairy_cheese == "Never" ~ 0,
    dairy_cheese == "1-2 days a week" ~ 1,
    dairy_cheese == "3-4 days a week" ~ 2,
    dairy_cheese == "5-6 days a week" ~ 3,
    dairy_cheese == "All Days" ~ 4
    ),
  dairy_paneer_points = case_when(
    dairy_paneer == "Never" ~ 0,
    dairy_paneer == "1-2 days a week" ~ 1,
    dairy_paneer == "3-4 days a week" ~ 2,
    dairy_paneer == "5-6 days a week" ~ 3,
    dairy_paneer == "All Days" ~ 4
    ),
  vegetables_points =case_when(
    vegetables == "Never" ~ 0,
    vegetables == "1-2 days a week" ~ 1,
    vegetables == "3-4 days a week" ~ 2,
    vegetables == "5-6 days a week" ~ 3,
    vegetables == "All Days" ~ 4
    ),
  fruits_points = case_when(
    fruits == "Never" ~ 0,
    fruits == "1-2 days a week" ~ 1,
    fruits == "3-4 days a week" ~ 2,
    fruits == "5-6 days a week" ~ 3,
    fruits == "All Days" ~ 4
    ),
  sugary_food_points = case_when(
    sugary_food == "Never" ~ 4,
    sugary_food == "1-2 days a week" ~ 4,
    sugary_food == "3-4 days a week" ~ 2,
    sugary_food == "5-6 days a week" ~ 1,
    sugary_food == "All Days" ~ 0
    ),
  fast_food_points = case_when(
    fast_food == "Never" ~ 4,
    fast_food == "1-2 days a week" ~ 4,
    fast_food == "3-4 days a week" ~ 2,
    fast_food == "5-6 days a week" ~ 1,
    fast_food == "All Days" ~ 0
    ),
  whole_grains_points = case_when(
    whole_grains == "Never" ~ 0,
    whole_grains == "1-2 days a week" ~ 1,
    whole_grains == "3-4 days a week" ~ 2,
    whole_grains == "5-6 days a week" ~ 3,
    whole_grains == "All Days" ~ 4
    ),
  protein_fish_points = case_when(
    protein_fish == "Never" ~ 0,
    protein_fish == "1-2 days a week" ~ 1,
    protein_fish == "3-4 days a week" ~ 2,
    protein_fish == "5-6 days a week" ~ 3,
    protein_fish == "All Days" ~ 4
    ),
  protein_chicken_points = case_when(
    protein_chicken == "Never" ~ 0,
    protein_chicken == "1-2 days a week" ~ 1,
    protein_chicken == "3-4 days a week" ~ 2,
    protein_chicken == "5-6 days a week" ~ 3,
    protein_chicken == "All Days" ~ 4
    ),
  protein_meat_points = case_when(
    protein_meat == "Never" ~ 0,
    protein_meat == "1-2 days a week" ~ 1,
    protein_meat == "3-4 days a week" ~ 2,
    protein_meat == "5-6 days a week" ~ 3,
    protein_meat == "All Days" ~ 4
    ),
  protein_eggs_points = case_when(
    protein_eggs == "Never" ~ 0,
    protein_eggs == "1-2 days a week" ~ 1,
    protein_eggs == "3-4 days a week" ~ 2,
    protein_eggs == "5-6 days a week" ~ 3,
    protein_eggs == "All Days" ~ 4
    ),
  protein_dal_points = case_when(
    protein_dal == "Never" ~ 0,
    protein_dal == "1-2 days a week" ~ 1,
    protein_dal == "3-4 days a week" ~ 2,
    protein_dal == "5-6 days a week" ~ 3,
    protein_dal == "All Days" ~ 4
    ),
  protein_nuts_points = case_when(
    protein_nuts == "Never" ~ 0,
    protein_nuts == "1-2 days a week" ~ 1,
    protein_nuts == "3-4 days a week" ~ 2,
    protein_nuts == "5-6 days a week" ~ 3,
    protein_nuts == "All Days" ~ 4
    ),
  water_points = case_when(
    water == "Rarely" ~ 2,
    water == "Mostly" ~ 4
    ),
  exercise_points = case_when(
    exercise == "None" ~ 0,
    exercise == "15 minutes" ~ 1,
    exercise == "30 minutes" ~ 2,
    exercise == "1 hour" ~ 4,
    exercise == "1 1/2 hour or more"~ 4
    ),
  screen_points = case_when(
    screen == "Never" ~ 4,
    screen == "15-60 minutes" ~ 4,
    screen == "1-2 hours" ~ 3,
    screen == "2-3 hours" ~ 2,
    screen == "3+ hours" ~ 1
    )
) 

#Calculating Points
cleaned_source_data <- mutate(source_data,
  protein_total_points = protein_chicken_points +protein_dal_points + protein_eggs_points + protein_fish_points + protein_meat_points + protein_nuts_points + whole_grains_points,
  dairy_total_points = dairy_milk_points + dairy_cheese_points + dairy_paneer_points +dairy_curd_points,

  dairy_variety_points = ((dairy_milk_points > 0) + (dairy_cheese_points  > 0) + (dairy_paneer_points  > 0) + (dairy_curd_points  > 0 )),
  
  healthy_points=sugary_food_points+ fast_food_points,

  dairy_total_p = case_when(
    dairy_total_points >= 7 ~ 10,
    dairy_total_points < 7 ~ dairy_total_points
    ),

  protein_total_p = case_when(
    protein_total_points >= 7 ~ 10,
    protein_total_points < 7 ~ protein_total_points
    )
)

#Calculating Total points
cleaned_source_data <- mutate(source_data,
  total_points = vegetables_points + fruits_points + protein_total_p + dairy_total_p + dairy_variety_points + screen_points + water_points + exercise_points + healthy_points
)

#Generating reports
iterator=1
for (name in source_data$first_name){
  subgroup <- source_data[source_data$first_name == name]
  render(input = "Pre-primary Report_RMD.Rmd", output_file = paste0(iterator, "_", name, "_", capitalize(tolower(subgroup$last_name))," ", "report", ".pdf"))
  iterator=iterator+1
}
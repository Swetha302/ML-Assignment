# Load necessary libraries
library(tidyverse)
library(tidymodels)
library(janitor)

# Read in the data (replace "studentInfo.csv" with your actual dataset file name)
students <- read_csv("studentInfo.csv")

# Data preprocessing
students <- students %>%
  mutate(pass_binary = ifelse(final_result == "Pass", 1, 0)) %>%
  mutate(pass = as.factor(pass_binary))

students <- students %>%
  mutate(disability_status = as.factor(disability))

# Example feature engineering
students <- students %>%
  mutate(imd_band_numeric = factor(imd_band, levels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", "50-60%", "60-70%", "70-80%", "80-90%", "90-100%"))) %>%
  mutate(imd_band_encoded = as.integer(imd_band_numeric))

# Split data into training and testing sets
set.seed(20230712)  # Set seed for reproducibility
train_test_split <- initial_split(students, prop = 0.80)
data_train <- training(train_test_split)
data_test <- testing(train_test_split)

# Create a recipe
my_rec <- recipe(pass ~ disability_status + imd_band_encoded, data = data_train)

# Specify the model
my_mod <- 
  logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

# Create and fit the workflow
my_wf <- 
  workflow() %>% 
  add_model(my_mod) %>% 
  add_recipe(my_rec)

fitted_model <- fit(my_wf, data = data_train)

# Evaluate the model using testing data
test_split <- rsample::initial_split(data_test, prop = 0.8)
final_fit <- last_fit(my_wf, split = test_split)

# View model performance
final_fit
final_fit %>%
  collect_predictions() %>%
  select(.pred_class, pass) %>%
  mutate(correct_prediction = .pred_class == pass) %>%
  tabyl(correct_prediction)



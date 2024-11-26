# Author: Madison Coots
# Date: November 26, 2024
# ----------------------
# This file makes diabetes risk predictions with various models.
# Coots et al. (2025)

library(tidyverse)
library(readr)
library(stringr)
library(here)
library(rstudioapi)

# Constructing data read and write paths
directory_path <- dirname(getActiveDocumentContext()$path)
data_object_read_path <- directory_path %>% str_remove("code/diabetes") %>% str_c("data/processed/diabetes_data.rds")
data_object_write_path <- directory_path %>% str_remove("code/diabetes") %>% str_c("data/processed/")
data <- readRDS(data_object_read_path)

# ======================================================================
# Predicting race-aware diabetes risk 
# ======================================================================

# This is our race-aware logistic regression model
race_aware_formula <- diabetes ~ (ridageyr*bmxbmi) + race + as.factor(ntile(data$bmxbmi, 10))
race_aware_model <- glm(race_aware_formula,
                        data = data,
                        family = "binomial",
                        weights = round(wtmec8yr/1000)) # glm complains when weights aren't ints
race_aware_model_pred <- predict(race_aware_model, newdata = data, type = "response")

# ======================================================================
# Predicting race-blind diabetes risk 
# ======================================================================

# This is our race-blind logistic regression model
race_blind_formula <- diabetes ~ (ridageyr*bmxbmi) + as.factor(ntile(data$bmxbmi, 10))
race_blind_model <- glm(race_blind_formula,
                        data = data,
                        family = "binomial",
                        weights = round(wtmec8yr/1000)) # glm complains when weights aren't ints
race_blind_model_pred <- predict(race_blind_model, newdata = data, type = "response")

# ======================================================================
# Model for approximating ground-truth diabetes risk 
# ======================================================================

# We use this race-aware model to approximate ground-truth diabetes incidence rates in subsequent analysis
large_model_formula <- diabetes ~ race + ridageyr + bmxbmi + gender +
  whd140 + bmxwt + bmxht + bmxwaist + relatives_had_diabetes + felt_depressed +
  income + health_insurance  + food_security

large_model <- glm(large_model_formula,
                   data = data,
                   family = "binomial",
                   weights = round(wtmec8yr/1000)) # glm complains when weights aren't ints

large_model_pred <- predict(large_model, newdata = data, type = "response")

# ======================================================================
# Model for smoothing diabetes utility curve
# ======================================================================

# We use this model to smooth out the utility curve in Figure 3
smoothed_model_formula <- diabetes ~ race*poly(race_blind_risk_score, 3)
smoothed_model <- glm(smoothed_model_formula,
                      data = data %>% mutate(race_blind_risk_score = race_blind_model_pred),
                      family = "binomial",
                      weights = round(wtmec8yr/1000))

smoothed_model_pred <- predict(smoothed_model, 
                               newdata = data %>% 
                                 mutate(race_blind_risk_score = race_blind_model_pred), 
                               type = "response")

# ======================================================================
# Kitchen sink diabetes risk model
# ======================================================================

# This is our kitchen sink race-blind model used to illustrate that the miscalibration
# issue is not resolved with the inclusion of additional covariates likely correlated
# with race
extended_race_blind_formula <- diabetes ~ (ridageyr*bmxbmi) + as.factor(ntile(data$bmxbmi, 10)) + gender +
  whd140 + bmxwt + bmxht + bmxwaist + relatives_had_diabetes + felt_depressed +
  income + health_insurance  + food_security

extended_race_blind_model <- glm(extended_race_blind_formula,
                                 data = data,
                                 family = "binomial",
                                 weights = round(wtmec8yr/1000)) # glm complains when weights aren't ints

extended_race_blind_model_pred <- predict(extended_race_blind_model, newdata = data, type = "response")

# ======================================================================
# Saving data and both predictions
# ======================================================================

all_data_with_risk <-
  data %>%
  mutate(race_aware_model_pred = race_aware_model_pred,
         race_blind_model_pred = race_blind_model_pred,
         large_model_pred = large_model_pred,
         smoothed_model_pred = smoothed_model_pred,
         extended_race_blind_model_pred = extended_race_blind_model_pred)

saveRDS(all_data_with_risk, file = paste(data_object_write_path, "diabetes_data_with_risk.rds", sep = ""))






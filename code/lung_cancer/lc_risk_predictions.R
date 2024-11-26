# Author: Madison Coots
# Date: November 26, 2024
# ----------------------
# This file makes lung cancer risk predictions.
# Coots et al. (2025)

library(tidyverse)
library(readr)
library(lcrisks)

read_path <- "~/Documents/harvard/research/lung_cancer/data/processed/lc_data.rds"
data_object_write_path <- "~/Documents/harvard/research/lung_cancer/data/processed/"
lc_weights_path <- "~/Documents/harvard/research/lung_cancer/data/processed/lc_data_weights.rds"

data <- readRDS(read_path)
weights <- readRDS(weights_path)

lcrisks_data <- data %>%
  select(age,
         female,
         years_smoked,
         years_quit,
         n_cig_per_day,
         race_code,
         lung_disease,
         num_relatives_with_lc,
         bmi,
         highest_educ_level,
         personal_cancer_history,
         hypertension,
         chd,
         angina,
         heart_attack,
         other_heart_disease,
         stroke,
         diab,
         bron,
         kidney,
         liver,
         spec_eq,
         year)

# ======================================================================
# Predicting race-aware lung cancer risk 
# ======================================================================

race_aware_risk <- lcrisk(lcrisks_data, nyears=5)
race_aware_lcrat <- race_aware_risk$`Number with lung cancer diagnosed per 1000 (LCRAT)` / 1000 

all_data_with_race_aware_risk <- data %>%
  mutate(race_aware_risk = race_aware_lcrat)

# ======================================================================
# Saving data and predictions (and attaching weights)
# ======================================================================

all_data_with_both_risk <-
  all_data_with_race_aware_risk %>%
  inner_join(weights, by = c("pid"))

saveRDS(all_data_with_both_risk, file = paste(data_object_write_path, "all_data_with_lc_risk.rds", sep = ""))


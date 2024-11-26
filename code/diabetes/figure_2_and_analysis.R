# Author: Madison Coots
# Date: November 26, 2024
# ======================
# Code for all figures, tables, and statistics.
# Coots et al. (2025)

library(tidyverse)
library(readr)
library(here)
library(rstudioapi)

directory_path <- dirname(getActiveDocumentContext()$path)

# Construct path for colors.R
colors_path <- directory_path %>% str_remove("diabetes") %>% str_c("colors.R")

# Construct path for diabetes data
data_object_read_path <- directory_path %>% 
  str_remove("code/diabetes") %>% 
  str_c("data/processed/diabetes_data_with_risk.rds")

# Construct path to save figures data
figure_save_path <- directory_path %>% 
  str_remove("code/diabetes") %>% 
  str_c("figures/")

# Load colors for figures
source(colors_path) 

# Load diabetes data
diabetes_data <- readRDS(data_object_read_path)

theme_set(theme_bw(base_size = 15))

risk_score_upper_bound <- 0.1
incidence_upper_bound <- 0.24
screening_thresh <- 0.015
optimal_thresh <- screening_thresh
screening_cost <- -screening_thresh
utility_reward <- 1
per_capita_factor <- 10000

# ===========================================================================================
# ======================================== Figure 2a ========================================
# ============================== Race-unaware calibration plot ================================
# ===========================================================================================

race_blind_calibration_plot <- function(diabetes_data,
                                        risk_score_upper_bound,
                                        incidence_upper_bound,
                                        screening_thresh) {
  race_blind_calibration_plot_data <- diabetes_data %>%
    mutate(risk_score = race_blind_model_pred,
           est_diabetes_prob = large_model_pred) %>%
    filter(!is.na(risk_score),
           !is.na(est_diabetes_prob)) %>%
    select(race, risk_score, est_diabetes_prob, wtmec8yr) %>%
    mutate(risk_score_bin = floor((risk_score  + 0.0025) * 100 * 2) / 2 / 100) %>% # round to the nearest 0.005
    group_by(race, risk_score_bin) %>%
    summarize(n_in_bin = sum(wtmec8yr),
              bin_avg_risk_score = sum(risk_score * wtmec8yr) / sum(wtmec8yr),
              diabetes_prev = sum(est_diabetes_prob * wtmec8yr) / sum(wtmec8yr))

  # This chunk determines the vertical order of the lines in the plot
  # so that we can have the order of the lines in the legend reflect
  # the order of the lines in the plot so that it is easier to read

  line_order <- race_blind_calibration_plot_data %>%
    # Make sure x-range lines up with what is visualized in plot
    filter(risk_score_bin < risk_score_upper_bound) %>%
    group_by(race) %>%
    summarize(mean_prev = mean(diabetes_prev)) %>%
    arrange(as.character(race)) %>%
    mutate(alph_index = row_number()) %>%
    arrange(desc(mean_prev)) %>%
    pull(alph_index)

  # This provides the color map in the right order for the legend
  ordered_group_color_map <- group_color_map[line_order]
  ordered_group_names <- group_names[line_order]

  race_blind_calibration_plot <- race_blind_calibration_plot_data %>%
    ggplot(aes(x=bin_avg_risk_score, y=diabetes_prev, color=race)) +
    geom_vline(xintercept=0.015) +
    geom_line() +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
    xlab("Race-unaware risk") +
    ylab("Observed diabetes rate") +
    scale_y_continuous(labels = scales::percent,
                       breaks = seq(0.0, incidence_upper_bound, 0.04)) +
    scale_x_continuous(labels = scales::percent,
                       breaks = seq(0.0, risk_score_upper_bound, 0.02)) +
    coord_cartesian(xlim = c(0, risk_score_upper_bound), ylim = c(0, incidence_upper_bound)) +
    theme(legend.title = element_blank(),
          legend.position = c(0.35, 0.84)) +
    scale_color_manual(values=ordered_group_color_map,
                       breaks = ordered_group_names)
  return(race_blind_calibration_plot)
}

# ===========================================================================================
# ======================================== Figure 2b ========================================
# ====================================== Scatter plot =======================================
# ===========================================================================================

race_blind_scatter_plot <- function(diabetes_data,
                                    risk_score_upper_bound,
                                    incidence_upper_bound,
                                    screening_thresh) {
  race_blind_scatter_plot <- diabetes_data %>%
    mutate(race_blind_risk = race_blind_model_pred,
           race_aware_risk = race_aware_model_pred) %>%
    drop_na(race_blind_model_pred, race_aware_model_pred) %>%
    ggplot(aes(x=race_blind_risk, y=race_aware_risk, color=race)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
    geom_point(shape = 1) +
    annotate("rect", xmin = -1, xmax = screening_thresh, ymin = -1, ymax = screening_thresh,
             alpha = 0.6, fill="lightgray") +
    annotate("rect", xmin = screening_thresh, xmax = 1, ymin = screening_thresh, ymax = 1,
             alpha = 0.6, fill="lightgray") +
    geom_vline(xintercept=screening_thresh) +
    geom_hline(yintercept=screening_thresh) +
    xlab("Race-unaware risk") +
    ylab("Race-aware risk") +
    scale_color_manual(values=group_color_map,
                       breaks =group_names) +
    theme(legend.title = element_blank(),
          legend.position = 'none') +
    coord_cartesian(xlim = c(0, risk_score_upper_bound), ylim = c(0, incidence_upper_bound)) +
    scale_y_continuous(labels = scales::percent,
                       breaks = seq(0.0, incidence_upper_bound, 0.04)) +
    scale_x_continuous(labels = scales::percent,
                       breaks = seq(0.0, risk_score_upper_bound, 0.02)) +
    guides(color = guide_legend(override.aes = list(shape = 21, fill = group_color_map)))
  return(race_blind_scatter_plot)
}

# ===========================================================================================
# ======================================== Figure 2c ========================================
# =================================== Utility dot plot ======================================
# ===========================================================================================

utility_dot_plot <- function(diabetes_data, screening_thresh, per_capita_factor) {
  relative_gains_by_group <-
    diabetes_data %>% 
    mutate(p_diabetes = large_model_pred,
           race_aware_risk_score = race_aware_model_pred,
           race_blind_risk_score = race_blind_model_pred,
           race_aware_decision = race_aware_risk_score > screening_thresh,
           race_blind_decision = race_blind_risk_score > optimal_thresh,
           expected_race_aware_utility = (screening_cost + utility_reward * p_diabetes) * race_aware_decision,
           expected_race_blind_utility = (screening_cost + utility_reward * p_diabetes) * race_blind_decision,
           utility_difference = expected_race_aware_utility - expected_race_blind_utility,
           baseline_utility = expected_race_blind_utility,
    ) %>%
    drop_na(baseline_utility, utility_difference) %>% # drop observations that have an NA prediction due to missing data
    group_by(race) %>%
    summarize(average_baseline_utility = sum(baseline_utility * wtmec8yr)/sum(wtmec8yr),
              average_utility_diff = sum(utility_difference * wtmec8yr)/sum(wtmec8yr),
              n = sum(wtmec8yr)) %>%
    mutate(pct_gain = average_utility_diff / average_baseline_utility)
  
  relative_gains_overall <- relative_gains_by_group %>%
    group_by() %>%
    summarize(pct_gain = sum(pct_gain * n) / sum(n)) %>%
    pull(pct_gain)
  
  dot_order <- relative_gains_by_group %>% pull(race)
  
  dot_plot <- relative_gains_by_group %>%
    ggplot(aes(x=pct_gain, y=factor(race, levels=dot_order), color=race, fill = race)) +
    geom_point(size=4) +
    geom_bar(stat="identity", width = 0.03) +
    geom_vline(xintercept=relative_gains_overall) +
    theme(legend.position="none") +
    xlab("Relative utility gain of race-aware predictions\nover race-unaware predictions") +
    ylab("Group") +
    scale_color_manual(values=group_color_map,
                       breaks =group_names) +
    scale_fill_manual(values=group_color_map,
                      breaks =group_names) +
    scale_x_continuous(labels = scales::percent,
                       minor_breaks = c()) + 
    scale_y_discrete(labels = NULL,
                     breaks = NULL)
  return(dot_plot)
}

# ===========================================================================================
# ======================================== Figure 2d ========================================
# ==================================== Dot plot % same ======================================
# ===========================================================================================

get_pct_changed_by_group <- function(diabetes_data, screening_thresh) {
  pct_changed_by_group <- diabetes_data %>%
    drop_na(race_blind_model_pred, race_aware_model_pred) %>%
    select(race, race_blind_model_pred, race_aware_model_pred, wtmec8yr) %>%
    mutate(race_blind_screening_decision = race_blind_model_pred > screening_thresh,
           race_aware_screening_decision = race_aware_model_pred > screening_thresh,
           changed_decision = (race_blind_screening_decision != race_aware_screening_decision)) %>%
    group_by(race) %>%
    summarize(pct_changed_decision = sum(changed_decision * wtmec8yr) / sum(wtmec8yr),
              n = sum(wtmec8yr)) %>%
    arrange(pct_changed_decision)
  return(pct_changed_by_group)
}

dot_plot <- function(diabetes_data, screening_thresh) {
  pct_changed_by_group <- get_pct_changed_by_group(diabetes_data, screening_thresh)
  pct_changed_overall <- pct_changed_by_group %>%
    group_by() %>%
    summarize(pct_changed_decision = sum(pct_changed_decision * n) / sum(n)) %>%
    pull(pct_changed_decision)

  dot_order <- pct_changed_by_group %>% pull(race)

  dot_plot <- pct_changed_by_group %>%
    ggplot(aes(x=pct_changed_decision, y=factor(race), color=race, fill = race)) +
    geom_point(size=4) +
    geom_bar(stat="identity", width = 0.03) +
    geom_vline(xintercept=pct_changed_overall) +
    theme(legend.position="none") +
    xlab("Fraction of individuals with changed decisions") +
    ylab("") +
    scale_color_manual(values=group_color_map,
                       breaks =group_names) +
    scale_fill_manual(values=group_color_map,
                      breaks =group_names) +
    scale_x_continuous(labels = scales::percent,
                       breaks = seq(0, 1, 0.25),
                       minor_breaks = c()) +
    scale_y_discrete(labels = NULL,
                     breaks = NULL) +
    coord_cartesian(xlim = c(0, 1))
  return(dot_plot)
}

# ===========================================================================================
# ==================================== Statistics ===========================================

# ============================== Overall relative utility gain ==============================

overall_relative_gain <- diabetes_data %>% 
  mutate(p_diabetes = large_model_pred,
         race_aware_risk_score = race_aware_model_pred,
         race_blind_risk_score = race_blind_model_pred,
         race_aware_decision = race_aware_risk_score > screening_thresh,
         race_blind_decision = race_blind_risk_score > optimal_thresh,
         expected_race_aware_utility = (screening_cost + utility_reward * p_diabetes) * race_aware_decision,
         expected_race_blind_utility = (screening_cost + utility_reward * p_diabetes) * race_blind_decision,
         utility_difference = expected_race_aware_utility - expected_race_blind_utility,
         baseline_utility = expected_race_blind_utility,
  ) %>%
  drop_na(baseline_utility, utility_difference) %>% # drop observations that have an NA prediction due to missing data
  summarize(average_baseline_utility = sum(baseline_utility * wtmec8yr)/sum(wtmec8yr),
            average_utility_diff = sum(utility_difference * wtmec8yr)/sum(wtmec8yr),
            n = sum(wtmec8yr)) %>%
  mutate(pct_gain = average_utility_diff / average_baseline_utility)

# ================= Overall fraction with unchanged decisions utility gain ==================

overall_pct_unchanged <- diabetes_data %>%
  drop_na(race_blind_model_pred, race_aware_model_pred) %>%
  select(race, race_blind_model_pred, race_aware_model_pred, wtmec8yr) %>%
  mutate(race_blind_screening_decision = race_blind_model_pred > screening_thresh,
         race_aware_screening_decision = race_aware_model_pred > screening_thresh,
         unchanged_decision = (race_blind_screening_decision == race_aware_screening_decision)) %>%
  summarize(pct_unchanged_decision = sum(unchanged_decision * wtmec8yr) / sum(wtmec8yr),
            n = sum(wtmec8yr)) %>%
  arrange(pct_unchanged_decision)


# ===========================================================================================
# =================================== Create all figures ====================================
# ===========================================================================================


diabetes_race_blind_calib <- race_blind_calibration_plot(diabetes_data,
                                                         risk_score_upper_bound,
                                                         incidence_upper_bound,
                                                         screening_thresh)

diabetes_race_blind_scatter <- race_blind_scatter_plot(diabetes_data,
                                                       risk_score_upper_bound,
                                                       incidence_upper_bound,
                                                       screening_thresh)

diabetes_dot_plot <- dot_plot(diabetes_data, screening_thresh)

diabetes_utility_dot_plot <- utility_dot_plot(diabetes_data, screening_thresh, per_capita_factor)

# ===========================================================================================
# ==================================== Save all figures =====================================
# ===========================================================================================

# Figure 2a
diabetes_race_blind_calib

ggsave(paste(figure_save_path, "figure_2a.pdf", sep = ""),
       width = 5,
       height = 5)

# Figure 2b
diabetes_race_blind_scatter

ggsave(paste(figure_save_path, "figure_2b.pdf", sep = ""),
       width = 5,
       height = 5)

# Figures 2c and 2d
dot_plots <- diabetes_utility_dot_plot + diabetes_dot_plot + 
  plot_layout(ncol = 2, nrow = 1)

ggsave(paste(figure_save_path, "figure_2c_d.pdf", sep = ""),
       width = 12,
       height = 2.5)

# Author: Madison Coots
# Date: November 26, 2024
# ======================
# Code for all lung cancer figures, tables, and statistics.
# Coots et al. (2025)

library(tidyverse)
library(readr)
library(here)
library(rstudioapi)
library(ggtext)

directory_path <- dirname(getActiveDocumentContext()$path)

# Construct path for colors.R
colors_path <- directory_path %>% str_remove("lung_cancer") %>% str_c("colors.R")

lung_cancer_data_path <- "~/Documents/harvard/research/lung_cancer/data/processed/all_data_with_lc_risk.rds"

# Construct path to save figures data
figure_save_path <- directory_path %>% 
  str_remove("code/lung_cancer") %>% 
  str_c("figures/")

# Load colors for figures
source(colors_path) 

# Load NLST data
data <- readRDS(lung_cancer_data_path) %>%
  rename(race = race_str) %>%
  mutate(race = if_else(race == "Asian/PI", "Asian", race),
         conflc = if_else(conflc == 1, TRUE, FALSE))

theme_set(theme_bw(base_size = 15))

screening_thresh <- 0.02

# ===========================================================================================
# =================== Computing thresholds for equalized decision rates =====================
# ===========================================================================================

marginal_plot_data <- data %>% 
  mutate(density = "marginal")

# The quantile that corresponds to 2% threshold on the White population
quantile_for_thresh_white_pop <- data %>%
  filter(race == "White") %>%
  mutate(above_thresh = race_aware_risk >= screening_thresh) %>%
  summarize(sum(above_thresh * weight) / sum(weight)) %>%
  pull()

# Race-specific thresholds that equalize decision rates across groups
race_group_thresh <- data %>%
  group_by(race) %>%
  arrange(desc(race_aware_risk)) %>%
  mutate(cum_proportion = cumsum(weight) / sum(weight),
         above_general_pop_quantile = cum_proportion <= quantile_for_thresh_white_pop) %>%
  summarize(race_group_thresh = min(race_aware_risk[above_general_pop_quantile])) %>%
  mutate(density = "All individuals")

# ===========================================================================================
# ===================== Computing thresholds for equalized error rates ======================
# ===========================================================================================

conditional_plot_data <- data %>%
  filter(conflc) %>%
  mutate(density = "conditional")

# FNR across whole population under normal screening threshold
fnr_whole_pop <- data %>%
  filter(conflc) %>%
  mutate(screening_decision = race_aware_risk >= screening_thresh,
         false_negative = !screening_decision) %>%
  summarize(sum(false_negative * weight) / sum(weight)) %>%
  pull()

# Race-specific thresholds that equalize FNR across groups
race_group_thresh_equalized_fnr <- data %>%
  filter(conflc) %>%
  group_by(race) %>%
  arrange(race_aware_risk) %>%
  mutate(fnr = cumsum(weight) / sum(weight),
         below_fnr_whole_pop = fnr <= fnr_whole_pop) %>%
  summarize(race_group_thresh = max(race_aware_risk[below_fnr_whole_pop])) %>%
  mutate(density = "Individuals with lung cancer")


# ===========================================================================================
# ======================================== Figure 1 =========================================
# ================================= LC Risk Distributions ===================================
# ===========================================================================================

plot_data <- bind_rows(marginal_plot_data, conditional_plot_data) %>%
  mutate(density = fct_recode(density, `All individuals` = "marginal", `Individuals with lung cancer` = "conditional"))

text_annotation <- data.frame(
  label = c("Expected <br>net **benefit**<br>from screening", "Expected <br>net **cost**<br>from screening"),
  x = c(screening_thresh + 0.001, screening_thresh - 0.001),
  race = c("Asian", "Asian"),
  y = 34,
  density = c("All individuals", "All individuals"),
  wrong = c("ok", "ok"),
  named_blind_screened = c("Screened", "Screened"),
  hjust = c(0, 1)
)

lc_distributions_plot <- plot_data %>%
  ggplot(aes(x = race_aware_risk, color = race)) +
  annotate("rect", xmin = screening_thresh, xmax = 1, ymin = -5, ymax = 40,
           alpha = .1)+
  geom_density(bw = 0.01, aes(weight = weight)) +
  geom_vline(xintercept = screening_thresh, show.legend = FALSE) +
  facet_wrap(vars(fct_rev(density))) + 
  xlab("Lung cancer risk") +
  ylab("Density") +
  geom_vline(data = race_group_thresh_equalized_fnr, aes(xintercept = race_group_thresh, color = race),
             linetype = "dashed", show.legend = FALSE) +
  geom_vline(data = race_group_thresh, aes(xintercept = race_group_thresh, color = race),
             linetype = "dashed", show.legend = FALSE) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous() +
  # Needed to use geom_richtext to enable 1) bolding part of the label, and adding the label to just one facet
  geom_richtext(data = text_annotation, mapping = aes(x = x, y = y, label = label, hjust = hjust),
                size = 4,
                fill = alpha(c("white"), 0.8),
                label.size = NA,
                color = "black") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.ticks.y = element_blank(),
        axis.text.y=element_blank(),
        panel.spacing = unit(1.5, "lines"),
        plot.margin = margin(10, 50, 10, 50, "pt"),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        strip.text.x = element_text(size = 17),
        axis.text.x = element_text(size = 15),
        legend.text = element_text(size = 17)) +
  scale_color_manual(values=group_color_map,
                     breaks = c("White", "Hispanic", "Asian", "Black")) +
  coord_cartesian(xlim = c(0, .1), ylim = c(0, 38)) +
  guides(color=guide_legend(
    keywidth=0.15,
    keyheight=0.15,
    default.unit="inch")
  )

# ===========================================================================================
# ==================================== Statistics ===========================================

# ===================== Fraction of each group normally above threshold =====================

data %>%
  mutate(above_thresh = race_aware_risk >= screening_thresh) %>%
  group_by(race) %>%
  summarize(pct = sum(above_thresh * weight) / sum(weight))

# =============== Group-specific risk thresholds that equalize decision rates ===============

race_group_thresh

# ===================== Fraction of each group normally above threshold =====================

# FNR by race with 2% threshold (for paper stats)
data %>%
  filter(conflc) %>%
  mutate(screening_decision = race_aware_risk >= screening_thresh,
         false_negative = !screening_decision) %>%
  group_by(race) %>%
  summarize(pct = sum(false_negative * weight) / sum(weight),
            n = sum(weight))

# ==================== Group-specific risk thresholds that equalize FNR =====================

race_group_thresh_equalized_fnr

# ===========================================================================================
# ==================================== Save all figures =====================================
# ===========================================================================================

# Figures 1a and 1b
lc_distributions_plot

ggsave(paste(figure_save_path, "figure_1_a_b.pdf", sep = ""),
       width = 315,
       height = 120,
       units = "mm")

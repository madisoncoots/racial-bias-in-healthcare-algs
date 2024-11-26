# Author: Madison Coots
# Date: November 26, 2024
# ======================
# Code for all figures. 
# Coots et al. (2025)

library(tidyverse)
library(rstudioapi)
library(here)
library(patchwork)

directory_path <- dirname(getActiveDocumentContext()$path)
setwd(directory_path)
figure_save_path <- directory_path %>% str_remove("code") %>% str_c("figures/")

figures_script_path <- here(directory_path, "figures_and_analysis.R")
# =============================================================================
# Run figures_and_analysis.R scripts to load the figures into the environment.
# =============================================================================
source(figures_script_path)

# =============================================================================
# Figure 2a/b: Race-blind calibration and scatter plots.
# =============================================================================

# Removing legends, axis labels, and adding facet labels for better presentation in a grid
diabetes_race_blind_calib <- diabetes_race_blind_calib +
  theme(legend.position="none") +
  xlab("Race-unaware risk") +
  ylab("Observed diabetes rate")

diabetes_race_blind_scatter <- diabetes_race_blind_scatter +
  theme(legend.position="right") +
  xlab("Race-unaware risk") +
  ylab("Race-aware risk")

calibration_scatter_plot <- 
  diabetes_race_blind_calib + 
  diabetes_race_blind_scatter +
  plot_layout(ncol = 2, nrow = 1)

ggsave(paste(figure_save_path, "calibration_scatter_plot.png", sep = ""),
       width = 9,
       height = 4)

ggsave(paste(figure_save_path, "calibration_scatter_plot.pdf", sep = ""),
       width = 9,
       height = 4)

# =============================================================================
# Figure 2c/d: Dot plots (utility and percent change)
# =============================================================================

diabetes_dot_plot_data <- ggplot_build(diabetes_dot_plot)$plot$data %>% 
  mutate(pop_average = sum(pct_changed_decision * n) / sum(n))

diabetes_utility_dot_plot_data <- ggplot_build(diabetes_utility_dot_plot)$plot$data %>%
  mutate(pop_average = sum(average_utility_diff * n) / sum(n))

dot_plot <- diabetes_dot_plot_data %>% 
  ggplot(aes(x=pct_changed_decision, y=race, color=race, fill = race)) +
  geom_point(size=4) +
  geom_bar(stat="identity", width = 0.03, show.legend = FALSE) +
  geom_vline(data = diabetes_dot_plot_data, aes(xintercept = pop_average), 
             show.legend = FALSE) +
  theme(legend.title = element_blank(),
        legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)) + 
  xlab("Fraction of individuals with\nchanged decisions") +
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

utility_dot_plot <- diabetes_utility_dot_plot_data %>% 
  ggplot(aes(x=pct_gain, y=race, color=race, fill = race)) +
  geom_point(size=4) +
  geom_bar(stat="identity", width = 0.03, show.legend = FALSE) +
  geom_vline(data = diabetes_utility_dot_plot_data, aes(xintercept = pop_average), 
             show.legend = FALSE) +
  theme(legend.title = element_blank(),
        legend.position = "none",
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)) + 
  xlab("Relative utility gain of\n race-aware predictions over\n race-unaware predictions") +
  ylab("Group") +
  scale_color_manual(values=group_color_map,
                     breaks =group_names) +
  scale_fill_manual(values=group_color_map,
                    breaks =group_names) +
  scale_x_continuous(labels = scales::percent,
                     minor_breaks = c()) + 
  scale_y_discrete(labels = NULL,
                   breaks = NULL)

dot_plots <- utility_dot_plot + dot_plot + 
  plot_layout(ncol = 2, nrow = 1)

ggsave(paste(figure_save_path, "dot_plots.png", sep = ""),
       width = 8,
       height = 2.5)

ggsave(paste(figure_save_path, "dot_plots.pdf", sep = ""),
       width = 8,
       height = 2.5)



# Part of Study 1: Semantic priming

# Combination of plots:
# 1. Interaction between gender and cosine similarity
# 2. Interaction between gender and visual rating difference

library(dplyr)
library(patchwork)
library(ggplot2)

# Data set below created in the script 'semanticpriming_data_preparation.R',
# which is stored in the folder 'semanticpriming/data'
semanticpriming = read.csv('semanticpriming/data/final_dataset/semanticpriming.csv')

# Set plain language labels
semanticpriming$participant_gender = 
  ifelse(semanticpriming$participant_gender == 'f', 'Female',
         ifelse(semanticpriming$participant_gender == 'F', 'Female',
                ifelse(semanticpriming$participant_gender == 'wf', 'Female',
                       ifelse(semanticpriming$participant_gender == 'm', 'Male',
                              ifelse(semanticpriming$participant_gender == 'M', 'Male',
                                     semanticpriming$participant_gender)))))

# Model below created in the script 'semanticpriming_lmerTest.R',
# which is stored in the folder 'semanticpriming/frequentist_analysis'
semanticpriming_lmerTest = 
  readRDS('semanticpriming/frequentist_analysis/results/semanticpriming_lmerTest.rds')

# Load custom function
source('R_functions/alias_interaction_plot.R')

plot1 =
  alias_interaction_plot(
    model = semanticpriming_lmerTest,
    dataset = semanticpriming,
    x = 'z_cosine_similarity',
    fill = 'z_recoded_participant_gender',
    fill_alias = 'participant_gender',
    fill_nesting_factor = 'Participant',
    x_title = 'Language-based similarity (*z*)',
    y_title = 'Predicted RT (*z*)',
    fill_title = 'Gender'
  ) +
  guides(color = 'none',
         fill = guide_legend(title = 'Gender', 
                             # In each key of the legend, replace the 
                             # default line with a full square.
                             override.aes = list(linetype = c(0, 0), 
                                                 alpha = 1), 
                             # Reverse default order of categories 
                             # in legend (else, Male would appear 
                             # first as it was coded as -0.5).
                             reverse = TRUE)) +
  theme(plot.tag.position = c(0, 1), legend.position = c(.78, .81))

plot2 = 
  alias_interaction_plot(
    model = semanticpriming_lmerTest,
    dataset = semanticpriming,
    x = 'z_visual_rating_diff',
    fill = 'z_recoded_participant_gender',
    fill_alias = 'participant_gender',
    fill_nesting_factor = 'Participant',
    x_title = 'Visual-strength difference (*z*)',
    y_title = 'Predicted RT (*z*)',
    fill_title = 'Gender'
  ) +
  guides(color = 'none',
         fill = guide_legend(title = 'Gender', 
                             # In each key of the legend, replace the 
                             # default line with a full square.
                             override.aes = list(linetype = c(0, 0), 
                                                 alpha = 1), 
                             # Reverse default order of categories 
                             # in legend (else, Male would appear 
                             # first as it was coded as -0.5).
                             reverse = TRUE)) +
  theme(plot.tag.position = c(0, 1), legend.position = 'none')

# Combine plots using {patchwork} and save the result to disk
( plot1 + plot2 + 
    plot_annotation(tag_levels = list(c('(a)', '(b)', '(c)'))) + 
    plot_layout(ncol = 1) ) %>%
  ggsave(filename = 'semanticpriming/frequentist_analysis/plots/semanticpriming-interactions-with-gender.pdf',
         device = cairo_pdf, width = 6, height = 7, dpi = 900)


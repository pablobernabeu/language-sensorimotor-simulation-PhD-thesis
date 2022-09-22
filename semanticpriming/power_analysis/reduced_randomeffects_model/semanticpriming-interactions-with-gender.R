

# Part of Study 1: Semantic priming

# Deprecated model. For details, please see README.md in the directory
# 'semanticpriming/power_analysis/reduced_randomeffects_model'

# Combination of plots:
# 1. Interaction between gender and cosine similarity
# 2. Interaction between gender and visual rating difference

library(dplyr)
library(patchwork)
library(ggplot2)

# Data set below created in the script 'semanticpriming_data_preparation.R',
# which is stored in the folder 'semanticpriming/data'
semanticpriming = 
  read.csv('semanticpriming/data/final_dataset/semanticpriming.csv')

# Consolidate gender labels
semanticpriming$participant_gender = 
  ifelse(semanticpriming$participant_gender == 'wf', 'Female',
         ifelse(semanticpriming$participant_gender == 'F', 'Female',
                ifelse(semanticpriming$participant_gender == 'f', 'Female',
                       ifelse(semanticpriming$participant_gender == 'M', 'Male',
                              ifelse(semanticpriming$participant_gender == 'm', 'Male',
                                     semanticpriming$participant_gender)))))

# Model below created in the script 'semanticpriming_lmerTest.R', which
# is stored in the folder 'semanticpriming/power_analysis/model'
semanticpriming_lmerTest = 
  readRDS('semanticpriming/power_analysis/model/results/semanticpriming_lmerTest.rds')

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
                             # In the legend, place labels in 
                             # alphabetical order.
                             reverse = TRUE)) +
  labs(tag = '(a)')

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
                             # In the legend, place labels in 
                             # alphabetical order.
                             reverse = TRUE)) +
  labs(tag = '(b)')

# Combine plots using {patchwork}
(plot1 + plot2 + plot_layout(ncol = 1, guides = 'collect')) %>%
  ggsave(filename = 'semanticpriming/power_analysis/reduced_randomeffects_model/plots/semanticpriming-interactions-with-gender.pdf',
         device = cairo_pdf, width = 6.5, height = 7, dpi = 900)


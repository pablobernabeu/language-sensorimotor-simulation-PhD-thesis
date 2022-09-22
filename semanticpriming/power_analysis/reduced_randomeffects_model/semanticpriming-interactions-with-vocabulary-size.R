

# Part of Study 1: Semantic priming

# Deprecated model. For details, please see README.md in the directory
# 'semanticpriming/power_analysis/reduced_randomeffects_model'

# Combination of plots:
# 1. Interaction between vocabulary size and language-based similarity
# 2. Interaction between vocabulary size and visual-strength difference

library(dplyr)
library(ggplot2)
library(patchwork)

# Data set below created in the script 'semanticpriming_data_preparation.R',
# which is stored in the folder 'semanticpriming/data'
semanticpriming = 
  read.csv('semanticpriming/data/final_dataset/semanticpriming.csv')

# Model below created in the script 'semanticpriming_lmerTest.R', which
# is stored in the folder 'semanticpriming/power_analysis/model'
semanticpriming_lmerTest = 
  readRDS('semanticpriming/power_analysis/model/results/semanticpriming_lmerTest.rds')

# Load custom function
source('R_functions/deciles_interaction_plot.R')

plot1 =
  deciles_interaction_plot(
    model = semanticpriming_lmerTest,
    x = 'z_cosine_similarity',
    fill = 'z_vocabulary_size',
    fill_nesting_factor = 'Participant',
    x_title = 'Language-based similarity (*z*)',
    y_title = 'Predicted RT (*z*)',
    fill_title = 'Vocabulary size<br>(*z*, deciles)'
  ) +
  labs(tag = '(a)')

plot2 =
  deciles_interaction_plot(
    model = semanticpriming_lmerTest,
    x = 'z_visual_rating_diff',
    fill = 'z_vocabulary_size',
    fill_nesting_factor = 'Participant',
    x_title = 'Visual-strength difference (*z*)',
    y_title = 'Predicted RT (*z*)',
    fill_title = 'Vocabulary size<br>(*z*, deciles)'
  ) +
  labs(tag = '(b)')

# Combine plots using {patchwork}
(plot1 + plot2 + plot_layout(ncol = 1, guides = 'collect')) %>%
  ggsave(filename = 'semanticpriming/power_analysis/reduced_randomeffects_model/plots/semanticpriming-interactions-with-vocabulary-size.pdf',
         device = cairo_pdf, width = 6.5, height = 7, dpi = 900)


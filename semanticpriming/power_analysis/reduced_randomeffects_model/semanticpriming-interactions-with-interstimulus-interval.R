

# Part of Study 1: Semantic priming

# Deprecated model. For details, please see README.md in the directory
# 'semanticpriming/power_analysis/reduced_randomeffects_model'

# Combination of plots:
# 1. Interaction between cosine similarity and interstimulus interval
# 2. Interaction between visual rating difference and interstimulus interval

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
source('R_functions/alias_interaction_plot.R')

plot1 =
  alias_interaction_plot(
    model = semanticpriming_lmerTest,
    dataset = semanticpriming,
    x = 'z_cosine_similarity',
    fill = 'z_recoded_interstimulus_interval',
    fill_alias = 'interstimulus_interval',
    x_title = 'Language-based similarity (*z*)',
    y_title = 'Predicted RT (*z*)',
    fill_title = 'Interstimulus<br>interval (ms)'
  ) +
  labs(tag = '(a)')

plot2 = 
  alias_interaction_plot(
    model = semanticpriming_lmerTest,
    dataset = semanticpriming,
    x = 'z_visual_rating_diff',
    fill = 'z_recoded_interstimulus_interval',
    fill_alias = 'interstimulus_interval',
    x_title = 'Visual-strength difference (*z*)',
    y_title = 'Predicted RT (*z*)',
    fill_title = 'Interstimulus<br>interval (ms)'
  ) +
  labs(tag = '(b)')

# Combine plots using {patchwork}
(plot1 + plot2 + plot_layout(ncol = 1, guides = 'collect')) %>%
  ggsave(filename = 'semanticpriming/power_analysis/reduced_randomeffects_model/plots/semanticpriming-interactions-with-interstimulus-interval.pdf',
         device = cairo_pdf, width = 6.5, height = 7, dpi = 900)


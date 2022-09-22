

# Part of Study 1: Semantic priming

# Combination of plots:
# 1. Interaction between vocabulary size and language-based similarity
# 2. Interaction between vocabulary size and visual-strength difference

library(dplyr)
library(ggplot2)
library(patchwork)

# Data set below created in the script 'semanticpriming_data_preparation.R',
# which is stored in the folder 'semanticpriming/data'
semanticpriming = read.csv('semanticpriming/data/final_dataset/semanticpriming.csv')

# Model below created in the script 'semanticpriming_lmerTest.R',
# which is stored in the folder 'semanticpriming/frequentist_analysis'
semanticpriming_lmerTest = 
  readRDS('semanticpriming/frequentist_analysis/results/semanticpriming_lmerTest.rds')

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
  ) + theme(plot.tag.position = c(0, 1))

plot2 =
  deciles_interaction_plot(
    model = semanticpriming_lmerTest,
    x = 'z_visual_rating_diff',
    fill = 'z_vocabulary_size',
    fill_nesting_factor = 'Participant',
    x_title = 'Visual-strength difference (*z*)',
    y_title = 'Predicted RT (*z*)',
    fill_title = 'Vocabulary size<br>(*z*, deciles)'
  ) + theme(plot.tag.position = c(0, 1))

# Combine plots using {patchwork} and save the result to disk
( plot1 + plot2 + 
    plot_annotation(tag_levels = list(c('(a)', '(b)'))) + 
    plot_layout(ncol = 1, guides = 'collect') ) %>%
  ggsave(filename = 'semanticpriming/frequentist_analysis/plots/semanticpriming-interactions-with-vocabulary-size.pdf',
         device = cairo_pdf, width = 6.5, height = 7, dpi = 900)




# Part of Study 3: Lexical decision

# Combination of plots:
# 1. Interaction between vocabulary age and word frequency
# 2. Interaction between vocabulary age and visual strength

library(dplyr)
library(ggplot2)
library(patchwork)

# Data set below created in the script 'lexicaldecision_data_preparation.R',
# which is stored in the folder 'lexicaldecision/data'
lexicaldecision = read.csv('lexicaldecision/data/final_dataset/lexicaldecision.csv')

# Model below created in the script 'lexicaldecision_lmerTest.R',
# which is stored in the folder 'lexicaldecision/frequentist_analysis'
lexicaldecision_lmerTest = 
  readRDS('lexicaldecision/frequentist_analysis/results/lexicaldecision_lmerTest.rds')

# Load custom function. Vocabulary age will be divided into sextiles, rather than 
# deciles, because there aren't enough estimates in the model to create deciles, 
# or even octiles.
source('R_functions/sextiles_interaction_plot.R')

plot1 =
  sextiles_interaction_plot(
    model = lexicaldecision_lmerTest,
    x = 'z_word_frequency',
    fill = 'z_vocabulary_age',
    fill_nesting_factor = 'Participant',
    x_title = 'Word frequency (*z*)',
    y_title = 'Predicted RT (*z*)',
    fill_title = 'Vocabulary age<br>(*z*, sextiles)'
  ) + theme(plot.tag.position = c(0, 1))

plot2 =
  sextiles_interaction_plot(
    model = lexicaldecision_lmerTest,
    x = 'z_visual_rating',
    fill = 'z_vocabulary_age',
    fill_nesting_factor = 'Participant',
    x_title = 'Visual strength (*z*)',
    y_title = 'Predicted RT (*z*)',
    fill_title = 'Vocabulary age<br>(*z*, sextiles)'
  ) + theme(plot.tag.position = c(0, 1))

# Combine plots using {patchwork} and save the result to disk
( plot1 + plot2 + 
    plot_annotation(tag_levels = list(c('(a)', '(b)'))) + 
    plot_layout(ncol = 1, guides = 'collect') ) %>%
  ggsave(filename = 'lexicaldecision/frequentist_analysis/plots/lexicaldecision-interactions-with-vocabulary-age.pdf',
         device = cairo_pdf, width = 6, height = 7, dpi = 900)


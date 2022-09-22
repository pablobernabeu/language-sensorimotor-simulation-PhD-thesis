

# Part of Study 3: Lexical decision

# 1. Interaction between word concreteness and vocabulary age
# 2. Interaction between word concreteness and gender

library(dplyr)
library(ggplot2)
library(patchwork)

# Data set below created in the script 'lexicaldecision_data_preparation.R',
# which is stored in the folder 'lexicaldecision/data'
lexicaldecision = read.csv('lexicaldecision/data/final_dataset/lexicaldecision.csv')

# Set plain language labels
lexicaldecision$participant_gender = 
  ifelse(lexicaldecision$participant_gender == 'f', 'Female',
         ifelse(lexicaldecision$participant_gender == 'm', 'Male', 
                ifelse(lexicaldecision$participant_gender == 'x', 'X', 
                       lexicaldecision$participant_gender)))

# Model below created in the script 'lexicaldecision_lmerTest.R',
# which is stored in the folder 'lexicaldecision/frequentist_analysis'
lexicaldecision_lmerTest = 
  readRDS('lexicaldecision/frequentist_analysis/results/lexicaldecision_lmerTest.rds')

# Load custom functions
source('R_functions/alias_interaction_plot.R')
source('R_functions/sextiles_interaction_plot.R')


# 1. Interaction between word concreteness and vocabulary age

sextiles_interaction_plot(
  model = lexicaldecision_lmerTest,
  x = 'z_word_concreteness',
  fill = 'z_vocabulary_age',
  fill_nesting_factor = 'Participant',
  x_title = 'Word concreteness (*z*)',
  y_title = 'Predicted RT (*z*)',
  fill_title = 'Vocabulary age<br>(*z*, sextiles)'
) %>%
  # save to disk
  ggsave(filename = 'lexicaldecision/frequentist_analysis/plots/lexicaldecision-interaction-word-concreteness-vocabulary-age.pdf',
         device = cairo_pdf, width = 7, height = 5.2, dpi = 900)


# 2. Interaction between word concreteness and gender

( alias_interaction_plot(
  model = lexicaldecision_lmerTest,
  dataset = lexicaldecision,
  x = 'z_word_concreteness',
  fill = 'z_recoded_participant_gender',
  fill_alias = 'participant_gender',
  fill_nesting_factor = 'Participant',
  x_title = 'Word concreteness (*z*)',
  y_title = 'Predicted RT (*z*)',
  fill_title = 'Gender'
) + theme(legend.position = c(.78, .83)) +
    guides(color = 'none',
           fill = guide_legend(title = 'Gender', 
                               # In each key of the legend, replace the 
                               # default line with a full square.
                               override.aes = list(linetype = c(0, 0, 0), 
                                                   alpha = 1), 
                               # Reverse default order of categories 
                               # in legend (else, Male would appear 
                               # first as it was coded as -0.5).
                               reverse = TRUE)) ) %>%
  # save to disk
  ggsave(filename = 'lexicaldecision/frequentist_analysis/plots/lexicaldecision-interaction-word-concreteness-gender.pdf',
         device = cairo_pdf, width = 5, height = 4.5, dpi = 900)






# Part of Study 2: Semantic decision

# Plot mean and 95% confidence interval for each effect

library(dplyr)
library(stringr)
library(ggplot2)

# Load custom function
source('R_functions/plot_95_confidence_intervals.R')

# Model below created in the script 'semanticdecision_lmerTest.R',
# which is stored in the folder 'semanticdecision/frequentist_analysis'

KR_summary_semanticdecision_lmerTest =   # Model summary with Kenward-Roger p values
  readRDS('semanticdecision/frequentist_analysis/results/KR_summary_semanticdecision_lmerTest.rds')

confint_semanticdecision_lmerTest =   # Confidence intervals
  readRDS('semanticdecision/frequentist_analysis/results/confint_semanticdecision_lmerTest.rds')

# Change labels of effects into plain language

rownames(KR_summary_semanticdecision_lmerTest$coefficients) =
  rownames(KR_summary_semanticdecision_lmerTest$coefficients) %>%
  
  # First, adjust names of variables (both in main effects and in interactions)
  str_replace(pattern = 'z_word_frequency',
              replacement = 'Word frequency') %>%
  str_replace(pattern = 'z_orthographic_Levenshtein_distance',
              replacement = 'Orthographic Levenshtein distance') %>%
  str_replace(pattern = 'z_word_concreteness',
              replacement = 'Word concreteness') %>%
  str_replace(pattern = 'z_word_cooccurrence',
              replacement = 'Word co-occurrence') %>%
  str_replace(pattern = 'z_visual_rating',
              replacement = 'Visual strength') %>%
  str_replace(pattern = 'z_information_uptake',
              replacement = 'Information uptake') %>%
  str_replace(pattern = 'z_vocabulary_size',
              replacement = 'Vocabulary size') %>%
  str_replace(pattern = 'z_recoded_participant_gender',
              replacement = 'Gender') %>%
  
  # Second, adjust order of effects in interactions. In the output from the model, 
  # the word-level variables of interest (i.e., 'z_word_cooccurrence' and 
  # 'z_visual_rating') sometimes appeared second in their interactions. For 
  # better consistency, the code below moves those word-level variables (with 
  # their new names) to the first position in their interactions. Note that the 
  # order does not affect the results in any way.
  sub('(\\w+.*):(Word co-occurrence|Visual strength)', '\\2:\\1', .)

rownames(confint_semanticdecision_lmerTest) =
  rownames(confint_semanticdecision_lmerTest) %>%
  
  # First, adjust names of variables (both in main effects and in interactions)
  str_replace(pattern = 'z_word_frequency',
              replacement = 'Word frequency') %>%
  str_replace(pattern = 'z_orthographic_Levenshtein_distance',
              replacement = 'Orthographic Levenshtein distance') %>%
  str_replace(pattern = 'z_word_concreteness',
              replacement = 'Word concreteness') %>%
  str_replace(pattern = 'z_word_cooccurrence',
              replacement = 'Word co-occurrence') %>%
  str_replace(pattern = 'z_visual_rating',
              replacement = 'Visual strength') %>%
  str_replace(pattern = 'z_information_uptake',
              replacement = 'Information uptake') %>%
  str_replace(pattern = 'z_vocabulary_size',
              replacement = 'Vocabulary size') %>%
  str_replace(pattern = 'z_recoded_participant_gender',
              replacement = 'Gender') %>%
  
  # Second, adjust order of effects in interactions. In the output from the model, 
  # the word-level variables of interest (i.e., 'z_word_cooccurrence' and 
  # 'z_visual_rating') sometimes appeared second in their interactions. For 
  # better consistency, the code below moves those word-level variables (with 
  # their new names) to the first position in their interactions. Note that the 
  # order does not affect the results in any way.
  sub('(\\w+.*):(Word co-occurrence|Visual strength)', '\\2:\\1', .)


( plot_95_confidence_intervals(
  KR_summary_semanticdecision_lmerTest, confint_semanticdecision_lmerTest, 
  x_title = 'Effect size (&beta;)', interaction_symbol_x = TRUE, 
  vertical_line_at_x = 0
) + theme(plot.margin = margin(9, 4, 14, 12)) ) %>%
  # Save plot
  ggsave(filename = 'semanticdecision/frequentist_analysis/plots/semanticdecision_confidence_intervals_plot.pdf', 
         device = cairo_pdf, width = 4.5, height = 6.2, dpi = 900)


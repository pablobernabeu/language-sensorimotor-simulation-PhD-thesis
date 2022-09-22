

# Part of Study 1: Semantic priming

# Plot mean and 95% confidence interval for each effect

library(dplyr)
library(stringr)
library(ggplot2)

# Load custom function
source('R_functions/plot_95_confidence_intervals.R')

# Model below created in the script 'semanticpriming_with_visualsimilarity_lmerTest.R',
# which is stored in the folder 'semanticpriming/analysis_with_visualsimilarity'

KR_summary_semanticpriming_with_visualsimilarity_lmerTest =   # Model summary with Kenward-Roger p values
  readRDS('semanticpriming/analysis_with_visualsimilarity/results/KR_summary_semanticpriming_with_visualsimilarity_lmerTest.rds')

confint_semanticpriming_with_visualsimilarity_lmerTest =   # Confidence intervals
  readRDS('semanticpriming/analysis_with_visualsimilarity/results/confint_semanticpriming_with_visualsimilarity_lmerTest.rds')

# Change labels of effects into plain language

rownames(KR_summary_semanticpriming_with_visualsimilarity_lmerTest$coefficients) =
  rownames(KR_summary_semanticpriming_with_visualsimilarity_lmerTest$coefficients) %>%
  
  # First, adjust names of variables (both in main effects and in interactions)
  str_replace(pattern = 'z_target_word_frequency',
              replacement = 'Target-word frequency') %>%
  str_replace(pattern = 'z_target_number_syllables',
              replacement = 'Number of target-word syllables') %>%
  str_replace(pattern = 'z_word_concreteness_diff',
              replacement = 'Word-concreteness difference') %>%
  str_replace(pattern = 'z_cosine_similarity',
              replacement = 'Language-based similarity') %>%
  str_replace(pattern = 'z_visual_rating_diff',
              replacement = 'Visual-strength difference') %>%
  str_replace(pattern = 'z_visual_similarity',
              replacement = 'Vision-based similarity') %>%
  str_replace(pattern = 'z_attentional_control',
              replacement = 'Attentional control') %>%
  str_replace(pattern = 'z_vocabulary_size',
              replacement = 'Vocabulary size') %>%
  str_replace(pattern = 'z_recoded_participant_gender',
              replacement = 'Gender') %>%
  str_replace(pattern = 'z_recoded_interstimulus_interval',
              replacement = 'SOA') %>%
  # Show acronym in main effect of SOA
  str_replace(pattern = '^SOA$',
              replacement = 'Stimulus onset asynchrony (SOA)') %>%
  
  # Second, adjust order of effects in interactions. In the output from the model, 
  # the word-level variables of interest (i.e., 'z_cosine_similarity' and 
  # 'z_visual_rating_diff') sometimes appeared second in their interactions. For 
  # better consistency, the code below moves those word-level variables (with 
  # their new names) to the first position in their interactions. Note that the 
  # order does not affect the results in any way.
  sub('(\\w+.*):(Language-based similarity|Visual-strength difference|Vision-based similarity)', 
      '\\2:\\1', 
      .)

rownames(confint_semanticpriming_with_visualsimilarity_lmerTest) =
  rownames(confint_semanticpriming_with_visualsimilarity_lmerTest) %>%
  
  # First, adjust names of variables (both in main effects and in interactions)
  str_replace(pattern = 'z_target_word_frequency',
              replacement = 'Target-word frequency') %>%
  str_replace(pattern = 'z_target_number_syllables',
              replacement = 'Number of target-word syllables') %>%
  str_replace(pattern = 'z_word_concreteness_diff',
              replacement = 'Word-concreteness difference') %>%
  str_replace(pattern = 'z_cosine_similarity',
              replacement = 'Language-based similarity') %>%
  str_replace(pattern = 'z_visual_rating_diff',
              replacement = 'Visual-strength difference') %>%
  str_replace(pattern = 'z_visual_similarity',
              replacement = 'Vision-based similarity') %>%
  str_replace(pattern = 'z_attentional_control',
              replacement = 'Attentional control') %>%
  str_replace(pattern = 'z_vocabulary_size',
              replacement = 'Vocabulary size') %>%
  str_replace(pattern = 'z_recoded_participant_gender',
              replacement = 'Gender') %>%
  str_replace(pattern = 'z_recoded_interstimulus_interval',
              replacement = 'SOA') %>%
  # Show acronym in main effect of SOA
  str_replace(pattern = '^SOA$',
              replacement = 'Stimulus onset asynchrony (SOA)') %>%
  
  # Second, adjust order of effects in interactions. In the output from the model, 
  # the word-level variables of interest (i.e., 'z_cosine_similarity' and 
  # 'z_visual_rating_diff') sometimes appeared second in their interactions. For 
  # better consistency, the code below moves those word-level variables (with 
  # their new names) to the first position in their interactions. Note that the 
  # order does not affect the results in any way.
  sub('(\\w+.*):(Language-based similarity|Visual-strength difference|Vision-based similarity)', 
      '\\2:\\1', 
      .)


( plot_95_confidence_intervals(
  KR_summary_semanticpriming_with_visualsimilarity_lmerTest, 
  confint_semanticpriming_with_visualsimilarity_lmerTest, 
  x_title = 'Effect size (&beta;)', interaction_symbol_x = TRUE, 
  axis_text_size = 13, vertical_line_at_x = 0,
  # Intercept and covariates commented out below
  select_effects = c(
    # '(Intercept)',
    # 'Attentional control',
    'Vocabulary size',
    'Gender',
    # 'Target-word frequency',
    # 'Number of target-word syllables',
    # 'Word-concreteness difference',
    'Language-based similarity',
    'Visual-strength difference',
    'Vision-based similarity',
    'Stimulus onset asynchrony (SOA)',
    # 'Word-concreteness difference:Vocabulary size',
    # 'Word-concreteness difference:SOA',
    # 'Word-concreteness difference:Gender',
    # 'Language-based similarity:Attentional control',
    # 'Visual-strength difference:Attentional control',
    # 'Vision-based similarity:Attentional control',
    'Language-based similarity:Vocabulary size',
    'Visual-strength difference:Vocabulary size',
    'Vision-based similarity:Vocabulary size',
    'Language-based similarity:Gender',
    'Visual-strength difference:Gender',
    'Language-based similarity:SOA',
    'Visual-strength difference:SOA',
    'Vision-based similarity:SOA'
  )
) + theme(plot.margin = margin(9, 4, 14, 12)) ) %>%
  # Save plot
  ggsave(filename = 'semanticpriming/analysis_with_visualsimilarity/plots/semanticpriming_with_visualsimilarity_confidence_intervals_plot.pdf', 
         device = cairo_pdf, width = 7, height = 7, dpi = 900)


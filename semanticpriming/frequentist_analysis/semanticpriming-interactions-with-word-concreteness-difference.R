

# Part of Study 1: Semantic priming

# 1. Interaction between word-concreteness difference and SOA
# 2. Interaction between word-concreteness difference and vocabulary size
# 3. Interaction between word-concreteness difference and gender

library(dplyr)
library(ggplot2)
library(patchwork)

# Data set below created in the script 'semanticpriming_data_preparation.R',
# which is stored in the folder 'semanticpriming/data'
semanticpriming = read.csv('semanticpriming/data/final_dataset/semanticpriming.csv')


## Convert interstimulus interval to stimulus onset asynchrony ##

# The stimulus onset asynchrony (SOA) has an alternative formula called the 
# 'interstimulus interval' (ISI). The difference between these is that the 
# ISI does not count the presentation of the prime word (example 
# equivalences between ISI and SOA are available ISI and SOA in Lam et al., 
# 2015, and in Figure 1A of Di Lollo et al., 2004). In the current study, 
# the presentation of the prime word lasts 150 ms. Thus, the 200-ms SOA is 
# equivalent to an ISI of 50 ms, and the 1,200-ms SOA corresponds to an 
# ISI of 1,050 ms (Hutchison et al., 2013). The use of either formula in 
# the analysis would not change our results, as we recoded the levels of 
# the factor as -0.5 and +0.5, and then z-scored those, following the 
# advice of Brauer and Curtin (2018). In our analyses (https://osf.io/ueryq), 
# we used the ISI formula, as it was the one present in the data set of 
# Hutchison et al. (2013; https://www.montana.edu/attmemlab/documents/all%20ldt%20subs_all%20trials3.xlsx). 
# However, we use the SOA formula in this paper as it has been more 
# commonly used in previous papers (e.g., Hutchison et al., 2013; Pecher
# et al., 1998; Petilli et al., 2021; Yap et al., 2017).

# The 'SOA' column is created below by replacing ISI values of 50 with 
# 200 and ISI values of 1050 with 1200.

semanticpriming$SOA = 
  ifelse(semanticpriming$interstimulus_interval == 50, 200, 
         ifelse(semanticpriming$interstimulus_interval == 1050, 1200, 
                semanticpriming$interstimulus_interval))


# Consolidate gender labels
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

# Load custom functions
source('R_functions/alias_interaction_plot.R')
source('R_functions/deciles_interaction_plot.R')


# 1. Interaction between word-concreteness difference and SOA

( alias_interaction_plot(
  model = semanticpriming_lmerTest,
  dataset = semanticpriming,
  x = 'z_word_concreteness_diff',
  fill = 'z_recoded_interstimulus_interval',
  fill_alias = 'SOA',
  x_title = 'Word-concreteness difference (*z*)',
  y_title = 'Predicted RT (*z*)',
  fill_title = 'SOA (ms)'
) + theme(legend.position = c(.88, .17)) ) %>%
  # save to disk
  ggsave(filename = 'semanticpriming/frequentist_analysis/plots/semanticpriming-interaction-word-concreteness-difference-SOA.pdf',
         device = cairo_pdf, width = 5, height = 4.5, dpi = 900)


# 2. Interaction between word-concreteness difference and vocabulary size

deciles_interaction_plot(
  model = semanticpriming_lmerTest,
  x = 'z_word_concreteness_diff',
  fill = 'z_vocabulary_size',
  fill_nesting_factor = 'Participant',
  x_title = 'Word-concreteness difference (*z*)',
  y_title = 'Predicted RT (*z*)',
  fill_title = 'Vocabulary size<br>(*z*, deciles)'
) %>%
  # save to disk
  ggsave(filename = 'semanticpriming/frequentist_analysis/plots/semanticpriming-interaction-word-concreteness-difference-vocabulary-size.pdf',
         device = cairo_pdf, width = 7, height = 5.2, dpi = 900)


# 3. Interaction between word-concreteness difference and gender

( alias_interaction_plot(
  model = semanticpriming_lmerTest,
  dataset = semanticpriming,
  x = 'z_word_concreteness_diff',
  fill = 'z_recoded_participant_gender',
  fill_alias = 'participant_gender',
  fill_nesting_factor = 'Participant',
  x_title = 'Word-concreteness difference (*z*)',
  y_title = 'Predicted RT (*z*)',
  fill_title = 'Gender'
) + theme(legend.position = c(.78, .19)) +
    guides(color = 'none',
           fill = guide_legend(title = 'Gender', 
                               # In each key of the legend, replace the 
                               # default line with a full square.
                               override.aes = list(linetype = c(0, 0), 
                                                   alpha = 1), 
                               # Reverse default order of categories 
                               # in legend (else, Male would appear 
                               # first as it was coded as -0.5).
                               reverse = TRUE)) ) %>%
  # save to disk
  ggsave(filename = 'semanticpriming/frequentist_analysis/plots/semanticpriming-interaction-word-concreteness-difference-gender.pdf',
         device = cairo_pdf, width = 5, height = 4.5, dpi = 900)




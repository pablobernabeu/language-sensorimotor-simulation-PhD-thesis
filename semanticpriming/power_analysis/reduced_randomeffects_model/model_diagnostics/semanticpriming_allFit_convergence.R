

# Part of Study 1: Semantic priming

# Deprecated model. For details, please see README.md in the directory
# 'semanticpriming/power_analysis/reduced_randomeffects_model'

# Convergence diagnostics

# Following Brauer and Curtin (2018), avoid removing random slopes to prevent an inflation of the Type I error rate.
# As a sanity check, refit mixed-effects model with several optimisers. This check is done by comparing the results 
# from these optimisers. If they are similar, especially for the fixed effects, then the results are likely valid,
# even in the presence of convergence warnings (see https://cran.r-project.org/web/packages/lme4/lme4.pdf).

library(dplyr)  # Data wrangling
library(dfoptim)  # Refit model with various optimisers using lme4::allFit()
library(optimx)  # Refit model with various optimisers using lme4::allFit()
library(lme4)   # Main analysis and adjustment of effect labels
library(parallel)  # Allow parallel processing using several cores

# Data set below created in the script 'semanticpriming_data_preparation.R',
# which is stored in the folder 'semanticpriming/data'
semanticpriming = 
  read.csv('semanticpriming/data/final_dataset/semanticpriming.csv')

# Model below created in the script 'semanticpriming_lmerTest.R', which
# is stored in the folder 'semanticpriming/power_analysis/model'
semanticpriming_lmerTest = 
  readRDS('semanticpriming/power_analysis/model/results/semanticpriming_lmerTest.rds')

semanticpriming_allFit_convergence =
  allFit(semanticpriming_lmerTest, 
         # Set maximum iterations to 1m to facilitate convergence 
         # (Brauer & Curtin, 2018; Singmann & Kellen, 2019)
         maxfun = 1e6,
         # Use 7 cores in parallel for faster computation
         ncpus = 7)

# Save (not saved because it takes up 2.2 GB)
# saveRDS(semanticpriming_allFit_convergence,
#         'semanticpriming/power_analysis/reduced_randomeffects_model/model_diagnostics/results/semanticpriming_allFit_convergence.rds')

################################################################################


# PLOT fixed effects from all the optimisers

# Adjust the names of the predictors in two ways, as detailed below
labels = colnames(semanticpriming_allFit_convergence[[1]]@pp$X) %>% 
  
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
  str_replace(pattern = 'z_attentional_control',
              replacement = 'Attentional control') %>%
  str_replace(pattern = 'z_vocabulary_size',
              replacement = 'Vocabulary size') %>%
  str_replace(pattern = 'z_recoded_participant_gender',
              replacement = 'Gender') %>%
  str_replace(pattern = 'z_recoded_interstimulus_interval',
              replacement = 'SOA') %>%
  
  # Second, adjust order of effects in interactions. In the output from the model, 
  # the word-level variables of interest (i.e., 'z_cosine_similarity' and 
  # 'z_visual_rating_diff') sometimes appeared second in their interactions. For 
  # better consistency, the code below moves those word-level variables (with 
  # their new names) to the first position in their interactions. Note that the 
  # order does not affect the results in any way.
  sub("(\\w+.*):(Language-based similarity|Visual-strength difference)", 
      '\\2:\\1', 
      .)

# The for-loop below iterates over the output from the seven optimisers. 
# In each iteration, the 'labels' created above are applied first, and
# then the 'pp' section of the 'allFit' output is created by passing 
# in the original content, of which only the column names of 'X' were 
# modified.

for(i in 1 : length(semanticpriming_allFit_convergence)) {
  X_temp = semanticpriming_allFit_convergence[[i]]@pp$X
  colnames(X_temp) = labels # <-- labels created above
  semanticpriming_allFit_convergence[[i]]@pp =
    merPredD(X = X_temp, 
             Zt = semanticpriming_allFit_convergence[[i]]@pp$Zt, 
             Lambdat = semanticpriming_allFit_convergence[[i]]@pp$Lambdat, 
             Lind = semanticpriming_allFit_convergence[[i]]@pp$Lind, 
             theta = semanticpriming_allFit_convergence[[i]]@pp$theta, 
             n = nrow(semanticpriming_allFit_convergence[[i]]@pp$X))
}

# Load function to plot fixed effects across different optimisers
source('R_functions/plot.fixef.allFit.R')

# Main effects
( plot.fixef.allFit(semanticpriming_allFit_convergence, 
                    # Select main effects, namely, predictors not containing a colon
                    select_predictors =
                      as.data.frame(labels) %>% filter(!str_detect(labels, ':')) %>% 
                      pull(labels),
                    y_title = 'Predicted RT (*z*)', decimal_points = 2, 
                    multiply_y_axis_limits = 1.3) ) %>%
  # Save plot
  ggsave(filename = 'semanticpriming/power_analysis/reduced_randomeffects_model/model_diagnostics/plots/main_effects_semanticpriming_allFit_convergence.pdf', 
         device = cairo_pdf, width = 9, height = 10.5, dpi = 900)

# Interactions
( plot.fixef.allFit(semanticpriming_allFit_convergence, 
                    # Select interactions, namely, predictors containing a colon
                    select_predictors = as.data.frame(labels) %>% 
                      filter(str_detect(labels, ':')) %>% pull(labels),
                    y_title = 'Predicted RT (*z*)', decimal_points = 3, 
                    multiply_y_axis_limits = 1.3) ) %>%
  # Save plot
  ggsave(filename = 'semanticpriming/power_analysis/reduced_randomeffects_model/model_diagnostics/plots/interactions_semanticpriming_allFit_convergence.pdf', 
         device = cairo_pdf, width = 9, height = 11.6, dpi = 900)

# Free up some memory
rm(semanticpriming_allFit_convergence)


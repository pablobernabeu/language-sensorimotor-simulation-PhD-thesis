

# Part of Study 2: Semantic decision

# Convergence diagnostics

# Following Brauer and Curtin (2018), avoid removing random slopes to prevent an inflation of the Type I error rate.
# As a sanity check, refit mixed-effects model with several optimisers. This check is done by comparing the results 
# from these optimisers. If they are similar, especially for the fixed effects, then the results are likely valid,
# even in the presence of convergence warnings (see https://cran.r-project.org/web/packages/lme4/lme4.pdf).


library(tidyverse)  # Data wrangling, text processing and plotting
library(dfoptim)  # Refit model with various optimisers using lme4::allFit()
library(optimx)  # Refit model with various optimisers using lme4::allFit()
library(lme4)   # Main analysis and adjustment of effect labels
library(parallel)  # Allow parallel processing using several cores


# Data set below created in the script 'semanticdecision_data_preparation.R',
# which is stored in the folder 'semanticdecision/data'
semanticdecision = 
  read.csv('semanticdecision/data/final_dataset/semanticdecision.csv')

# Model below created in the script 'semanticdecision_lmerTest.R',
# which is stored in the folder 'semanticdecision/frequentist_analysis'
semanticdecision_lmerTest = 
  readRDS('semanticdecision/frequentist_analysis/results/semanticdecision_lmerTest.rds')

# Fit model using the seven available optimisers
semanticdecision_allFit_convergence = 
  allFit(semanticdecision_lmerTest, 
         # Set maximum iterations to 1m to facilitate convergence 
         # (Brauer & Curtin, 2018; Singmann & Kellen, 2019)
         maxfun = 1e6,
         # Use 7 cores in parallel for faster computation
         ncpus = 7)

# Save
saveRDS(semanticdecision_allFit_convergence,
        'semanticdecision/frequentist_analysis/model_diagnostics/results/semanticdecision_allFit_convergence.rds')

# Load the result back in, if needed later
# semanticdecision_allFit_convergence =
#   readRDS('semanticdecision/frequentist_analysis/model_diagnostics/results/semanticdecision_allFit_convergence.rds')

################################################################################


# PLOT fixed effects from all the optimisers

# Adjust the names of the predictors in two ways, as detailed below
labels = colnames(semanticdecision_allFit_convergence[[1]]@pp$X) %>% 
  
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

# The for-loop below iterates over the output from the seven optimisers. 
# In each iteration, the 'labels' created above are applied first, and
# then the 'pp' section of the 'allFit' output is created by passing 
# in the original content, of which only the column names of 'X' were 
# modified.

for(i in 1 : length(semanticdecision_allFit_convergence)) {
  X_temp = semanticdecision_allFit_convergence[[i]]@pp$X
  colnames(X_temp) = labels # <-- labels created above
  semanticdecision_allFit_convergence[[i]]@pp =
    merPredD(X = X_temp, 
             Zt = semanticdecision_allFit_convergence[[i]]@pp$Zt, 
             Lambdat = semanticdecision_allFit_convergence[[i]]@pp$Lambdat, 
             Lind = semanticdecision_allFit_convergence[[i]]@pp$Lind, 
             theta = semanticdecision_allFit_convergence[[i]]@pp$theta, 
             n = nrow(semanticdecision_allFit_convergence[[i]]@pp$X))
}

# Load function to plot fixed effects across different optimisers
source('R_functions/plot.fixef.allFit.R')

# Main effects
( plot.fixef.allFit(semanticdecision_allFit_convergence, 
                    # Select main effects, namely, predictors not containing a colon
                    select_predictors = as.data.frame(labels) %>% 
                      filter(!str_detect(labels, ':')) %>% pull(labels),
                    y_title = 'Predicted RT (*z*)', decimal_points = 2, 
                    multiply_y_axis_limits = 1.3) ) %>%
  # Save plot
  ggsave(filename = 'semanticdecision/frequentist_analysis/model_diagnostics/plots/main_effects_semanticdecision_allFit_convergence.pdf', 
         device = cairo_pdf, width = 9, height = 8.5, dpi = 900)

# Interactions
( plot.fixef.allFit(semanticdecision_allFit_convergence, 
                    # Select interactions, namely, predictors containing a colon
                    select_predictors = as.data.frame(labels) %>% 
                      filter(str_detect(labels, ':')) %>% pull(labels),
                    y_title = 'Predicted RT (*z*)', decimal_points = 3, 
                    multiply_y_axis_limits = 1.3) ) %>%
  # Save plot
  ggsave(filename = 'semanticdecision/frequentist_analysis/model_diagnostics/plots/interactions_semanticdecision_allFit_convergence.pdf', 
         device = cairo_pdf, width = 9, height = 8.5, dpi = 900)

# Free up some memory
rm(semanticdecision_allFit_convergence)


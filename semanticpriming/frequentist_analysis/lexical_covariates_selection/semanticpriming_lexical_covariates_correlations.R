
# Part of Study 1: Semantic priming

# Zero-order correlations between lexical covariates


# install.packages('dplyr', repos = 'https://www.stats.bris.ac.uk/R/')
library(dplyr)    # Data wrangling

# install.packages('Cairo', repos = 'https://www.stats.bris.ac.uk/R/')
library(Cairo)    # Allows use of special characters such as dashes in plots

# install.packages('ggplot2', repos = 'https://www.stats.bris.ac.uk/R/')
library(ggplot2)    # ggsave()


# Read in data
semanticpriming = 
  read.csv('semanticpriming/data/final_dataset/semanticpriming.csv')

# Load correlation matrix function
source('R_functions/correlation_matrix.R')

# Using the following variables...
(semanticpriming[, c('z_target_phonological_Levenshtein_distance', 
                     'z_target_orthographic_Levenshtein_distance', 
                     'z_target_word_frequency', 'z_target_length', 
                     'z_target_number_syllables')] %>%
    
    # renamed for the sake of clarity
    rename('Target-word PLD20' = z_target_phonological_Levenshtein_distance,
           'Target-word OLD20' = z_target_orthographic_Levenshtein_distance,
           'Target-word frequency' = z_target_word_frequency,
           'Target-word length' = z_target_length,
           'Number of target-word syllables' = z_target_number_syllables) %>%
    
    # make correlation matrix (custom function from 'R_functions' folder)
    correlation_matrix() + 
    
    theme(plot.margin = unit(c(0.1, 0.5, 0.1, -0.8), 'in'))) %>%
  
  # Save
  ggsave(filename = 'semanticpriming/frequentist_analysis/lexical_covariates_selection/plots/semanticpriming_lexical_covariates_correlations.pdf',
         device = cairo_pdf, width = 8, height = 6, units = 'in', dpi = 500)



# Part of Study 2: Semantic decision

# Zero-order correlations between lexical covariates


# install.packages('dplyr', repos = 'https://www.stats.bris.ac.uk/R/')
library(dplyr)    # Data wrangling

# install.packages('Cairo', repos = 'https://www.stats.bris.ac.uk/R/')
library(Cairo)    # Allows use of special characters such as dashes in plots

# install.packages('ggplot2', repos = 'https://www.stats.bris.ac.uk/R/')
library(ggplot2)    # ggsave()


# Read in data
semanticdecision = 
  read.csv('semanticdecision/data/final_dataset/semanticdecision.csv')

# Load correlation matrix function
source('R_functions/correlation_matrix.R')

# Using the following variables...
(semanticdecision[, c('z_phonological_Levenshtein_distance', 
                      'z_orthographic_Levenshtein_distance', 
                      'z_word_frequency', 'z_word_length', 
                      'z_number_syllables')] %>%
    
    # renamed for the sake of clarity
    rename('PLD20' = z_phonological_Levenshtein_distance,
           'OLD20' = z_orthographic_Levenshtein_distance,
           'Word frequency' = z_word_frequency,
           'Word length' = z_word_length,
           'Number of syllables' = z_number_syllables) %>%
    
    # make correlation matrix (custom function from 'R_functions' folder)
    correlation_matrix() + 
    
    theme(plot.margin = unit(c(0.1, 0.6, 0.1, -1.76), 'in'))) %>%
  
  # Save
  ggsave(filename = 'semanticdecision/frequentist_analysis/lexical_covariates_selection/plots/semanticdecision_lexical_covariates_correlations.pdf',
         device = cairo_pdf, width = 8, height = 6, units = 'in', dpi = 500)


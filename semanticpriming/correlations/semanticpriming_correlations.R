

# Part of Study 1: Semantic priming

# Zero-order correlations between continuous variables


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
(semanticpriming[, c('z_target.RT', 'z_vocabulary_size', 
                     'z_attentional_control',  'z_cosine_similarity', 
                     'z_visual_rating_diff', 'z_word_concreteness_diff', 
                     'z_target_word_frequency', 
                     'z_target_number_syllables')] %>%
    
    # renamed for the sake of clarity
    rename('RT' = z_target.RT, 
           'Vocabulary size' = z_vocabulary_size,
           'Attentional control' = z_attentional_control,
           'Language-based similarity' = z_cosine_similarity,
           'Visual-strength difference' = z_visual_rating_diff,
           'Word-concreteness difference' = z_word_concreteness_diff,
           'Target-word frequency' = z_target_word_frequency,
           'Number of target-word syllables' = z_target_number_syllables) %>%
    
    # make correlation matrix (custom function from 'R_functions' folder)
    correlation_matrix() + 
    
    theme(plot.margin = unit(c(0.1, 0.6, 0.1, -1.76), 'in'))) %>%
  
  # Save
  ggsave(filename = 'semanticpriming/correlations/plots/semanticpriming_with_visualsimilarity_correlations.pdf',
         device = cairo_pdf, width = 8, height = 6, units = 'in', dpi = 500)


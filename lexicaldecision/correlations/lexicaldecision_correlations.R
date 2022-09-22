

# Part of Study 3: Lexical decision

# Zero-order correlations between continuous variables


# install.packages('dplyr', repos = 'https://www.stats.bris.ac.uk/R/')
library(dplyr)    # Data wrangling

# install.packages('Cairo', repos = 'https://www.stats.bris.ac.uk/R/')
library(Cairo)    # Allows use of special characters such as dashes in plots

# install.packages('ggplot2', repos = 'https://www.stats.bris.ac.uk/R/')
library(ggplot2)    # ggsave()


# Read in data
lexicaldecision = 
  read.csv('lexicaldecision/data/final_dataset/lexicaldecision.csv')

# Load correlation matrix function
source('R_functions/correlation_matrix.R')

# Using the following variables...
(lexicaldecision[, c('z_RT', 'z_vocabulary_age', 'z_word_frequency', 
                     'z_visual_rating', 'z_word_concreteness', 
                     'z_orthographic_Levenshtein_distance')] %>%
    
    # renamed for the sake of clarity
    rename('RT' = z_RT, 
           'Vocabulary age' = z_vocabulary_age,
           'Word frequency' = z_word_frequency,
           'Visual strength' = z_visual_rating,
           'Word concreteness' = z_word_concreteness,
           'OLD20' = z_orthographic_Levenshtein_distance) %>%
    
    # make correlation matrix (custom function from 'R_functions' folder)
    correlation_matrix() + 
    
    theme(plot.margin = unit(c(0.1, 0.6, 0.1, -1.76), 'in'))) %>%
  
  # Save
  ggsave(filename = 'lexicaldecision/correlations/plots/lexicaldecision_correlations.pdf',
         device = cairo_pdf, width = 8, height = 6, units = 'in', dpi = 500)


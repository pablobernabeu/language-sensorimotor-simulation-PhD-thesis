

# Part of Study 3: Lexical decision

# Combination of plots:
# 1. Interaction between gender and word frequency
# 2. Interaction between gender and visual strength

library(dplyr)
library(forcats)
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

# Set levels before plotting
lexicaldecision$participant_gender = lexicaldecision$participant_gender %>%
  as.factor() %>% factor(levels = c('Female', 'Male', 'X'))

# Model below created in the script 'lexicaldecision_lmerTest.R',
# which is stored in the folder 'lexicaldecision/frequentist_analysis'
lexicaldecision_lmerTest = 
  readRDS('lexicaldecision/frequentist_analysis/results/lexicaldecision_lmerTest.rds')

# Load custom function
source('R_functions/alias_interaction_plot.R')

plot1 =
  alias_interaction_plot(
    model = lexicaldecision_lmerTest,
    dataset = lexicaldecision,
    x = 'z_word_frequency',
    fill = 'z_recoded_participant_gender',
    fill_alias = 'participant_gender',
    fill_nesting_factor = 'Participant',
    x_title = 'Word frequency (*z*)',
    y_title = 'Predicted RT (*z*)',
    fill_title = 'Gender'
  ) +
  guides(color = 'none',
         fill = guide_legend(title = 'Gender', 
                             # In each key of the legend, replace the 
                             # default line with a full square.
                             override.aes = list(linetype = c(0, 0, 0), 
                                                 alpha = 1), 
                             # Reverse default order of categories 
                             # in legend (else, Male would appear 
                             # first as it was coded as -0.5).
                             reverse = TRUE)) +
  theme(plot.tag.position = c(0, 1), 
        legend.position = c(.82, .82))

plot2 =
  alias_interaction_plot(
    model = lexicaldecision_lmerTest,
    dataset = lexicaldecision,
    x = 'z_visual_rating',
    fill = 'z_recoded_participant_gender',
    fill_alias = 'participant_gender',
    fill_nesting_factor = 'Participant',
    x_title = 'Visual strength (*z*)',
    y_title = 'Predicted RT (*z*)',
    fill_title = 'Gender'
  ) +
  guides(color = 'none',
         fill = guide_legend(title = 'Gender', 
                             # In each key of the legend, replace the 
                             # default line with a full square.
                             override.aes = list(linetype = c(0, 0, 0), 
                                                 alpha = 1), 
                             # Reverse default order of categories 
                             # in legend (else, Male would appear 
                             # first as it was coded as -0.5).
                             reverse = TRUE)) +
  theme(plot.tag.position = c(0, 1), 
        legend.position = 'none')

# Combine plots using {patchwork} and save the result to disk
( plot1 + plot2 + 
    plot_annotation(tag_levels = list(c('(a)', '(b)'))) + 
    plot_layout(ncol = 1) ) %>%
  ggsave(filename = 'lexicaldecision/frequentist_analysis/plots/lexicaldecision-interactions-with-gender.pdf',
         device = cairo_pdf, width = 6.5, height = 7, dpi = 900)


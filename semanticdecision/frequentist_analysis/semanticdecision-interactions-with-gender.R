

# Part of Study 2: Semantic decision

# Combination of plots:
# 1. Interaction between gender and word co-occurrence
# 2. Interaction between gender and visual strength
# 3. Interaction between gender and word concreteness

library(dplyr)
library(ggplot2)
library(patchwork)

# Data set below created in the script 'semanticdecision_data_preparation.R',
# which is stored in the folder 'semanticdecision/data'
semanticdecision = read.csv('semanticdecision/data/final_dataset/semanticdecision.csv')

# Set plain language labels
semanticdecision$participant_gender = 
  ifelse(semanticdecision$participant_gender == 'F', 'Female',
         ifelse(semanticdecision$participant_gender == 'M', 'Male', 
                semanticdecision$participant_gender))

# Model below created in the script 'semanticdecision_lmerTest.R',
# which is stored in the folder 'semanticdecision/frequentist_analysis'
semanticdecision_lmerTest = 
  readRDS('semanticdecision/frequentist_analysis/results/semanticdecision_lmerTest.rds')

# Load custom function
source('R_functions/alias_interaction_plot.R')

plot1 =
  alias_interaction_plot(
    model = semanticdecision_lmerTest,
    dataset = semanticdecision,
    x = 'z_word_cooccurrence',
    fill = 'z_recoded_participant_gender',
    fill_alias = 'participant_gender',
    fill_nesting_factor = 'Participant',
    x_title = "Word co-occurrence (*z*)",
    y_title = 'Predicted RT (*z*)',
    fill_title = 'Gender'
  ) +
  guides(color = 'none',
         fill = guide_legend(title = 'Gender', 
                             # In each key of the legend, replace the 
                             # default line with a full square.
                             override.aes = list(linetype = c(0, 0), 
                                                 alpha = 1), 
                             # Reverse default order of categories 
                             # in legend (else, Male would appear 
                             # first as it was coded as -0.5).
                             reverse = TRUE)) +
  theme(plot.tag.position = c(0, 1), 
        legend.key.width = unit(1.2, 'cm'),
        legend.margin = margin(15, 15, 15, 15))

plot2 =
  alias_interaction_plot(
    model = semanticdecision_lmerTest,
    dataset = semanticdecision,
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
                             override.aes = list(linetype = c(0, 0), 
                                                 alpha = 1), 
                             # Reverse default order of categories 
                             # in legend (else, Male would appear 
                             # first as it was coded as -0.5).
                             reverse = TRUE)) +
  theme(axis.title.y = element_blank(), 
        legend.key.width = unit(1.2, 'cm'),
        legend.margin = margin(15, 15, 15, 15))

plot3 =
  alias_interaction_plot(
    model = semanticdecision_lmerTest,
    dataset = semanticdecision,
    x = 'z_word_concreteness',
    fill = 'z_recoded_participant_gender',
    fill_alias = 'participant_gender',
    fill_nesting_factor = 'Participant',
    x_title = 'Word concreteness (*z*)',
    y_title = 'Predicted RT (*z*)',
    fill_title = 'Gender'
  ) +
  guides(color = 'none',
         fill = guide_legend(title = 'Gender', 
                             # In each key of the legend, replace the 
                             # default line with a full square.
                             override.aes = list(linetype = c(0, 0), 
                                                 alpha = 1), 
                             # Reverse default order of categories 
                             # in legend (else, Male would appear 
                             # first as it was coded as -0.5).
                             reverse = TRUE)) +
  theme(plot.tag.position = c(0, 1), 
        legend.key.width = unit(1.2, 'cm'),
        legend.margin = margin(15, 15, 15, 15))

# Combine plots using {patchwork} and save the result to disk
( plot1 + plot2 + plot3 + 
    plot_annotation(tag_levels = list(c('(a)', '(b)', '(c)'))) + 
    guide_area() + plot_layout(ncol = 2, guides = 'collect') ) %>%
  ggsave(filename = 'semanticdecision/frequentist_analysis/plots/semanticdecision-interactions-with-gender.pdf',
         device = cairo_pdf, width = 7.5, height = 7, dpi = 900)


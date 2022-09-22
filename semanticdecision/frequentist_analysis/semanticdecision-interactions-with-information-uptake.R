

# Part of Study 2: Semantic decision

# Combination of plots:
# 1. Interaction between information uptake and word co-occurrence
# 2. Interaction between information uptake and visual strength
# 3. Interaction between information uptake and word concreteness

library(dplyr)
library(ggplot2)
library(patchwork)

# Data set below created in the script 'semanticdecision_data_preparation.R',
# which is stored in the folder 'semanticdecision/data'
semanticdecision = read.csv('semanticdecision/data/final_dataset/semanticdecision.csv')

# Model below created in the script 'semanticdecision_lmerTest.R',
# which is stored in the folder 'semanticdecision/frequentist_analysis'
semanticdecision_lmerTest = 
  readRDS('semanticdecision/frequentist_analysis/results/semanticdecision_lmerTest.rds')

# Load custom function
source('R_functions/deciles_interaction_plot.R')

plot1 =
  deciles_interaction_plot(
    model = semanticdecision_lmerTest,
    x = 'z_word_cooccurrence',
    fill = 'z_information_uptake',
    fill_nesting_factor = 'Participant',
    x_title = "Word co-occurrence (*z*)",
    y_title = 'Predicted RT (*z*)',
    fill_title = 'Information uptake (*z*, deciles)'
  ) + theme(plot.tag.position = c(0, 1), 
            legend.key.width = unit(1.2, 'cm'),
            legend.margin = margin(t = 21),
            legend.background = element_blank())

plot2 =
  deciles_interaction_plot(
    model = semanticdecision_lmerTest,
    x = 'z_visual_rating',
    fill = 'z_information_uptake',
    fill_nesting_factor = 'Participant',
    x_title = 'Visual strength (*z*)',
    y_title = 'Predicted RT (*z*)',
    fill_title = 'Information uptake (*z*, deciles)'
  ) + theme(plot.tag.position = c(0, 1), 
            axis.title.y = element_blank(), 
            legend.key.width = unit(1.2, 'cm'),
            legend.margin = margin(t = 21),
            legend.background = element_blank())

plot3 =
  deciles_interaction_plot(
    model = semanticdecision_lmerTest,
    x = 'z_word_concreteness',
    fill = 'z_information_uptake',
    fill_nesting_factor = 'Participant',
    x_title = 'Word concreteness (*z*)',
    y_title = 'Predicted RT (*z*)',
    fill_title = 'Information uptake (*z*, deciles)'
  ) + theme(plot.tag.position = c(0, 1), 
            legend.key.width = unit(1.2, 'cm'),
            legend.margin = margin(t = 21),
            legend.background = element_blank())

# Combine plots using {patchwork} and save the result to disk
( plot1 + plot2 + plot3 + 
    plot_annotation(tag_levels = list(c('(a)', '(b)', '(c)'))) + 
    guide_area() + plot_layout(ncol = 2, guides = 'collect') ) %>%
  ggsave(filename = 'semanticdecision/frequentist_analysis/plots/semanticdecision-interactions-with-information-uptake.pdf',
         device = cairo_pdf, width = 7.5, height = 7, dpi = 900)


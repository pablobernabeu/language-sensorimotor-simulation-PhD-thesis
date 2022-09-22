

# Part of Study 1: Semantic priming

# Combination of plots:
# 1. Interaction between language-based similarity and SOA
# 2. Interaction between vision-based similarity and SOA
# 3. Interaction between visual-strength difference and SOA

library(dplyr)
library(ggplot2)
library(patchwork)

# Data set below created in the script 'semanticpriming_with_visualsimilarity_data_preparation.R',
# which is stored in the folder 'semanticpriming/data'
semanticpriming_with_visualsimilarity = 
  read.csv('semanticpriming/data/subset_with_visualsimilarity/semanticpriming_with_visualsimilarity.csv')

# Model below created in the script 'semanticpriming_with_visualsimilarity_lmerTest.R',
# which is stored in the folder 'semanticpriming/analysis_with_visualsimilarity'
semanticpriming_with_visualsimilarity_lmerTest = 
  readRDS('semanticpriming/analysis_with_visualsimilarity/results/semanticpriming_with_visualsimilarity_lmerTest.rds')

# Load custom function
source('R_functions/alias_interaction_plot.R')

plot1 =
  alias_interaction_plot(
    model = semanticpriming_with_visualsimilarity_lmerTest,
    dataset = semanticpriming_with_visualsimilarity,
    x = 'z_cosine_similarity',
    fill = 'z_recoded_interstimulus_interval',
    fill_alias = 'interstimulus_interval',
    x_title = 'Language-based similarity (*z*)',
    y_title = 'Predicted RT (*z*)',
    fill_title = 'SOA (ms)'
  ) +
  theme(plot.tag.position = c(0, 1))

plot2 = 
  alias_interaction_plot(
    model = semanticpriming_with_visualsimilarity_lmerTest,
    dataset = semanticpriming_with_visualsimilarity,
    x = 'z_visual_rating_diff',
    fill = 'z_recoded_interstimulus_interval',
    fill_alias = 'interstimulus_interval',
    x_title = 'Visual-strength difference (*z*)',
    y_title = 'Predicted RT (*z*)',
    fill_title = 'SOA (ms)'
  ) +
  theme(axis.title.y = element_blank())

plot3 = 
  alias_interaction_plot(
    model = semanticpriming_with_visualsimilarity_lmerTest,
    dataset = semanticpriming_with_visualsimilarity,
    x = 'z_visual_similarity',
    fill = 'z_recoded_interstimulus_interval',
    fill_alias = 'interstimulus_interval',
    x_title = 'Vision-based similarity (*z*)',
    y_title = 'Predicted RT (*z*)',
    fill_title = 'SOA (ms)'
  ) +
  theme(plot.tag.position = c(0, 1))

# Combine plots using {patchwork} and save the result to disk
( plot1 + plot2 + plot3 + 
    plot_annotation(tag_levels = list(c('(a)', '(b)', '(c)'))) + 
    guide_area() + plot_layout(ncol = 2, guides = 'collect') ) %>%
  ggsave(filename = 'semanticpriming/analysis_with_visualsimilarity/plots/semanticpriming_with_visualsimilarity-interactions-with-SOA.pdf',
         device = cairo_pdf, width = 7, height = 7, dpi = 900)


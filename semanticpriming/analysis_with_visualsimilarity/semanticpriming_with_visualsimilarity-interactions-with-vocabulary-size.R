

# Part of Study 1: Semantic priming

# Combination of plots:
# 1. Interaction between vocabulary size and language-based similarity
# 2. Interaction between vocabulary size and vision-based similarity
# 3. Interaction between vocabulary size and visual-strength difference

library(dplyr)
library(ggplot2)
library(patchwork)

# Model below created in the script 'semanticpriming_with_visualsimilarity_lmerTest.R',
# which is stored in the folder 'semanticpriming/analysis_with_visualsimilarity'
semanticpriming_with_visualsimilarity_lmerTest = 
  readRDS('semanticpriming/analysis_with_visualsimilarity/results/semanticpriming_with_visualsimilarity_lmerTest.rds')

# Load custom function
source('R_functions/deciles_interaction_plot.R')

plot1 =
  deciles_interaction_plot(
    model = semanticpriming_with_visualsimilarity_lmerTest,
    x = 'z_cosine_similarity', 
    fill = 'z_vocabulary_size',
    fill_nesting_factor = 'Participant',
    x_title = 'Language-based similarity (*z*)',
    y_title = 'Predicted RT (*z*)',
    fill_title = 'Vocabulary size (*z*, deciles)'
  ) +
  theme(plot.tag.position = c(0, 1), 
        legend.key.width = unit(1.2, 'cm'),
        legend.margin = margin(t = 21),
        legend.background = element_blank())

plot2 =
  deciles_interaction_plot(
    model = semanticpriming_with_visualsimilarity_lmerTest,
    x = 'z_visual_rating_diff', 
    fill = 'z_vocabulary_size',
    fill_nesting_factor = 'Participant',
    x_title = 'Visual-strength difference (*z*)',
    y_title = 'Predicted RT (*z*)',
    fill_title = 'Vocabulary size (*z*, deciles)'
  ) +
  theme(axis.title.y = element_blank(), 
        legend.key.width = unit(1.2, 'cm'),
        legend.margin = margin(t = 21),
        legend.background = element_blank())

plot3 =
  deciles_interaction_plot(
    model = semanticpriming_with_visualsimilarity_lmerTest,
    x = 'z_visual_similarity', 
    fill = 'z_vocabulary_size',
    fill_nesting_factor = 'Participant',
    x_title = 'Vision-based similarity (*z*)',
    y_title = 'Predicted RT (*z*)',
    fill_title = 'Vocabulary size (*z*, deciles)'
  ) +
  theme(plot.tag.position = c(0, 1), 
        legend.key.width = unit(1.2, 'cm'),
        legend.margin = margin(t = 21),
        legend.background = element_blank())

# Combine plots using {patchwork} and save the result to disk
( plot1 + plot2 + plot3 + 
    plot_annotation(tag_levels = list(c('(a)', '(b)', '(c)'))) + 
    guide_area() + plot_layout(ncol = 2, guides = 'collect') ) %>%
  ggsave(filename = 'semanticpriming/analysis_with_visualsimilarity/plots/semanticpriming_with_visualsimilarity-interactions-with-vocabulary-size.pdf',
         device = cairo_pdf, width = 7, height = 7, dpi = 900)


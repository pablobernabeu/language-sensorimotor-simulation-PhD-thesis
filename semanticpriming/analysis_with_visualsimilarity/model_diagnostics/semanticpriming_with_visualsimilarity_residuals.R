

# Part of Study 1: Semantic priming

# Normality diagnostics: Frequency distribution of the residuals

library(dplyr)

# Read in model
semanticpriming_with_visualsimilarity_lmerTest = 
  readRDS('semanticpriming/analysis_with_visualsimilarity/results/semanticpriming_with_visualsimilarity_lmerTest.rds')

source('R_functions/residuals_plot.R')

# Save plot
residuals_plot(semanticpriming_with_visualsimilarity_lmerTest) %>%
  ggsave(filename = 'semanticpriming/analysis_with_visualsimilarity/model_diagnostics/results/plots/semanticpriming_residuals.png', 
         type = 'cairo-png', width = 7, height = 7, units = 'in', dpi = 800)



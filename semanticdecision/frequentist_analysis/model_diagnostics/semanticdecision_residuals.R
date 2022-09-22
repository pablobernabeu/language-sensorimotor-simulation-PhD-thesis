

# Part of Study 2: Semantic decision

# Normality diagnostics: Frequency distribution of the residuals

library(dplyr)

# Read in model
semanticdecision_lmerTest = 
  readRDS('semanticdecision/frequentist_analysis/results/semanticdecision_lmerTest.rds')

source('R_functions/residuals_plot.R')

# Save plot
residuals_plot(semanticdecision_lmerTest) %>%
  ggsave(filename = 'semanticdecision/frequentist_analysis/model_diagnostics/results/plots/semanticdecision_residuals.png', 
         type = 'cairo-png', width = 7, height = 7, units = 'in', dpi = 800)



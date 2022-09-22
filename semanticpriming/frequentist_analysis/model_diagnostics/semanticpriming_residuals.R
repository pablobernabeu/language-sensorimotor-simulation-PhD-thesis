

# Part of Study 1: Semantic priming

# Normality diagnostics: Frequency distribution of the residuals

library(dplyr)

# Read in model
semanticpriming_lmerTest = 
  readRDS('semanticpriming/frequentist_analysis/results/semanticpriming_lmerTest.rds')

source('R_functions/residuals_plot.R')

# Save plot
residuals_plot(semanticpriming_lmerTest) %>%
  ggsave(filename = 'semanticpriming/frequentist_analysis/model_diagnostics/results/plots/semanticpriming_residuals.png', 
         type = 'cairo-png', width = 7, height = 7, units = 'in', dpi = 800)



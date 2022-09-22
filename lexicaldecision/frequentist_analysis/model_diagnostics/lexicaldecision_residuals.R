

# Part of Study 3: Lexical decision

# Normality diagnostics: Frequency distribution of the residuals

library(dplyr)

# Read in model
lexicaldecision_lmerTest = 
  readRDS('lexicaldecision/frequentist_analysis/results/lexicaldecision_lmerTest.rds')

source('R_functions/residuals_plot.R')

# Save plot
residuals_plot(lexicaldecision_lmerTest) %>%
  ggsave(filename = 'lexicaldecision/frequentist_analysis/model_diagnostics/results/plots/lexicaldecision_residuals.png', 
         type = 'cairo-png', width = 7, height = 7, units = 'in', dpi = 800)





# Part of Study 3: Lexical decision

# Presenting the frequentist and the Bayesian estimates in the same plot. 
# The method used is to add the frequentist results into a plot from 
# brms::mcmc_plot()


library(tidyverse)
library(ggtext)
library(patchwork)
library(Cairo)

# Load custom function to present the frequentist and 
# the Bayesian estimates in the same plot
source('R_functions/frequentist_bayesian_plot.R')

# Load frequentist coefficients (estimates and confidence intervals)

KR_summary_lexicaldecision_lmerTest = 
  readRDS('lexicaldecision/frequentist_analysis/results/KR_summary_lexicaldecision_lmerTest.rds')

confint_lexicaldecision_lmerTest = 
  readRDS('lexicaldecision/frequentist_analysis/results/confint_lexicaldecision_lmerTest.rds')

# Below are the default names of the effects
rownames(KR_summary_lexicaldecision_lmerTest$coefficients)
rownames(confint_lexicaldecision_lmerTest)

# Load Bayesian posterior distributions

lexicaldecision_posteriordistributions_informativepriors_exgaussian = 
  readRDS('lexicaldecision/bayesian_analysis/results/lexicaldecision_posteriordistributions_informativepriors_exgaussian.rds')

lexicaldecision_posteriordistributions_weaklyinformativepriors_exgaussian = 
  readRDS('lexicaldecision/bayesian_analysis/results/lexicaldecision_posteriordistributions_weaklyinformativepriors_exgaussian.rds')

lexicaldecision_posteriordistributions_diffusepriors_exgaussian = 
  readRDS('lexicaldecision/bayesian_analysis/results/lexicaldecision_posteriordistributions_diffusepriors_exgaussian.rds')

# Below are the default names of the effects
levels(lexicaldecision_posteriordistributions_diffusepriors_exgaussian$data$parameter)

# Create a vector containing the names of the effects. This vector will be passed 
# to the plotting function.

new_labels = 
  
  lexicaldecision_posteriordistributions_diffusepriors_exgaussian$data$parameter %>% 
  
  unique %>%
  
  # Remove the default 'b_' from the beginning of each effect
  str_remove('^b_') %>%
  
  # Put Intercept in parentheses
  str_replace(pattern = 'Intercept', replacement = '(Intercept)') %>%
  
  # Adjust names of variables (both in main effects and in interactions)
  str_replace(pattern = 'z_orthographic_Levenshtein_distance',
              replacement = 'Orthographic Levenshtein distance') %>%
  str_replace(pattern = 'z_word_concreteness',
              replacement = 'Word concreteness') %>%
  str_replace(pattern = 'z_word_frequency',
              replacement = 'Word frequency') %>%
  str_replace(pattern = 'z_visual_rating',
              replacement = 'Visual strength') %>%
  str_replace(pattern = 'z_vocabulary_age',
              replacement = 'Vocabulary age') %>%
  str_replace(pattern = 'z_recoded_participant_gender',
              replacement = 'Gender') %>%
  
  # Adjust order of effects in interactions. In the original model output, the
  # word-level variables (i.e., 'z_word_frequency' and 'z_visual_rating') 
  # sometimes appeared second in their interactions. For better consistency, 
  # the code below moves those word-level variables (with their new names) to 
  # the first position in their interactions. Note that the order does not 
  # affect the results in any way.
  sub('(\\w+.*):(Word frequency|Visual strength)', '\\2:\\1', .) %>%
  
  # Replace colons denoting interactions with times symbols
  str_replace(pattern = ':', replacement = ' &times; ')


# Create plots, beginning with the informative-prior model

plot_lexicaldecision_frequentist_bayesian_plot_informativepriors_exgaussian =
  frequentist_bayesian_plot(KR_summary_lexicaldecision_lmerTest,
                            confint_lexicaldecision_lmerTest,
                            lexicaldecision_posteriordistributions_informativepriors_exgaussian,
                            labels = new_labels, interaction_symbol_x = TRUE,
                            vertical_line_at_x = 0, x_title = 'Effect size (&beta;)', 
                            x_axis_labels = 3, note_frequentist_no_prior = TRUE) +
  ggtitle('Prior *SD* = 0.1')

#####

# Save single plot
( frequentist_bayesian_plot(KR_summary_lexicaldecision_lmerTest,
                            confint_lexicaldecision_lmerTest,
                            lexicaldecision_posteriordistributions_weaklyinformativepriors_exgaussian,
                            labels = new_labels, interaction_symbol_x = TRUE,
                            vertical_line_at_x = 0, x_title = 'Effect size (&beta;)',
                            legend_ncol = 1) + 
    theme(legend.position = 'bottom') ) %>%
  ggsave(filename = 'lexicaldecision/frequentist_bayesian_plots/plots/lexicaldecision_frequentist_bayesian_plot_weaklyinformativepriors_exgaussian.pdf',
         device = cairo_pdf, width = 5.4, height = 6, dpi = 900)

# Plot adjusted to fit in the combination of all three plots
plot_lexicaldecision_frequentist_bayesian_plot_weaklyinformativepriors_exgaussian =
  frequentist_bayesian_plot(KR_summary_lexicaldecision_lmerTest,
                            confint_lexicaldecision_lmerTest,
                            lexicaldecision_posteriordistributions_weaklyinformativepriors_exgaussian,
                            labels = new_labels, interaction_symbol_x = TRUE,
                            vertical_line_at_x = 0, x_title = 'Effect size (&beta;)', 
                            x_axis_labels = 3, note_frequentist_no_prior = TRUE) +
  ggtitle('Prior *SD* = 0.2') +
  theme(axis.text.y = element_blank())

#####

plot_lexicaldecision_frequentist_bayesian_plot_diffusepriors_exgaussian =
  frequentist_bayesian_plot(KR_summary_lexicaldecision_lmerTest,
                            confint_lexicaldecision_lmerTest,
                            lexicaldecision_posteriordistributions_diffusepriors_exgaussian,
                            labels = new_labels, interaction_symbol_x = TRUE,
                            vertical_line_at_x = 0, x_title = 'Effect size (&beta;)', 
                            x_axis_labels = 3, note_frequentist_no_prior = TRUE) +
  ggtitle('Prior *SD* = 0.3') + 
  theme(axis.text.y = element_blank())

#####


# Save combination of the three plots

( plot_lexicaldecision_frequentist_bayesian_plot_informativepriors_exgaussian +
    plot_lexicaldecision_frequentist_bayesian_plot_weaklyinformativepriors_exgaussian +
    plot_lexicaldecision_frequentist_bayesian_plot_diffusepriors_exgaussian +
    
    plot_layout(ncol = 3, guides = 'collect') & theme(legend.position = 'bottom') ) %>%
  
  ggsave(filename = 'lexicaldecision/frequentist_bayesian_plots/plots/lexicaldecision_frequentist_bayesian_plot_allpriors_exgaussian.pdf',
         device = cairo_pdf, width = 7.4, height = 6.1, dpi = 900)


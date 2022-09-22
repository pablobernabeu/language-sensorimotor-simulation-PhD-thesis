

# Part of Study 2: Semantic decision

# Presenting the frequentist and the Bayesian estimates in the same plot. 
# For this purpose, the frequentist results are merged into a plot from 
# brms::mcmc_plot()


library(tidyverse)
library(ggtext)
library(patchwork)
library(Cairo)

# Load custom function to present the frequentist and 
# the Bayesian estimates in the same plot
source('R_functions/frequentist_bayesian_plot.R')

# Load frequentist coefficients (estimates and confidence intervals)

KR_summary_semanticdecision_lmerTest = 
  readRDS('semanticdecision/frequentist_analysis/results/KR_summary_semanticdecision_lmerTest.rds')

confint_semanticdecision_lmerTest = 
  readRDS('semanticdecision/frequentist_analysis/results/confint_semanticdecision_lmerTest.rds')

# Below are the default names of the effects
rownames(KR_summary_semanticdecision_lmerTest$coefficients)
rownames(confint_semanticdecision_lmerTest)

# Load Bayesian posterior distributions

semanticdecision_posteriordistributions_informativepriors_exgaussian = 
  readRDS('semanticdecision/bayesian_analysis/results/semanticdecision_posteriordistributions_informativepriors_exgaussian.rds')

semanticdecision_posteriordistributions_weaklyinformativepriors_exgaussian =
  readRDS('semanticdecision/bayesian_analysis/results/semanticdecision_posteriordistributions_weaklyinformativepriors_exgaussian.rds')

semanticdecision_posteriordistributions_diffusepriors_exgaussian = 
  readRDS('semanticdecision/bayesian_analysis/results/semanticdecision_posteriordistributions_diffusepriors_exgaussian.rds')

# Below are the default names of the effects
levels(semanticdecision_posteriordistributions_diffusepriors_exgaussian$data$parameter)


# Create a vector containing the names of the effects. This vector will be passed 
# to the plotting function.

new_labels = 
  
  semanticdecision_posteriordistributions_diffusepriors_exgaussian$data$parameter %>% 
  unique %>%
  
  # Remove the default 'b_' from the beginning of each effect
  str_remove('^b_') %>%
  
  # Put Intercept in parentheses
  str_replace(pattern = 'Intercept', replacement = '(Intercept)') %>%
  
  # First, adjust names of variables (both in main effects and in interactions)
  str_replace(pattern = 'z_word_frequency',
              replacement = 'Word frequency') %>%
  str_replace(pattern = 'z_orthographic_Levenshtein_distance',
              replacement = 'Orthographic Levenshtein distance') %>%
  str_replace(pattern = 'z_word_concreteness',
              replacement = 'Word concreteness') %>%
  str_replace(pattern = 'z_word_cooccurrence',
              replacement = 'Word co-occurrence') %>%
  str_replace(pattern = 'z_visual_rating',
              replacement = 'Visual strength') %>%
  str_replace(pattern = 'z_information_uptake',
              replacement = 'Information uptake') %>%
  str_replace(pattern = 'z_vocabulary_size',
              replacement = 'Vocabulary size') %>%
  str_replace(pattern = 'z_recoded_participant_gender',
              replacement = 'Gender') %>%
  
  # Second, adjust order of effects in interactions. In the output from the model, 
  # the word-level variables of interest (i.e., 'z_cosine_similarity' and 
  # 'z_visual_rating_diff') sometimes appeared second in their interactions. For 
  # better consistency, the code below moves those word-level variables (with 
  # their new names) to the first position in their interactions. Note that the 
  # order does not affect the results in any way.
  sub('(\\w+.*):(Word co-occurrence|Visual strength)', '\\2:\\1', .) %>%
  
  # Replace colons denoting interactions with times symbols
  str_replace(pattern = ':', replacement = ' &times; ')


# Create plots, beginning with the informative-prior model

plot_semanticdecision_frequentist_bayesian_plot_informativepriors_exgaussian =
  frequentist_bayesian_plot(KR_summary_semanticdecision_lmerTest,
                            confint_semanticdecision_lmerTest,
                            semanticdecision_posteriordistributions_informativepriors_exgaussian,
                            labels = new_labels, interaction_symbol_x = TRUE,
                            vertical_line_at_x = 0, x_title = 'Effect size (&beta;)', 
                            x_axis_labels = 3, note_frequentist_no_prior = TRUE) +
  ggtitle('Prior *SD* = 0.1')

#####

# Save single plot
( frequentist_bayesian_plot(KR_summary_semanticdecision_lmerTest,
                            confint_semanticdecision_lmerTest,
                            semanticdecision_posteriordistributions_weaklyinformativepriors_exgaussian,
                            labels = new_labels, interaction_symbol_x = TRUE,
                            vertical_line_at_x = 0, x_title = 'Effect size (&beta;)',
                            legend_ncol = 1) + 
    theme(legend.position = 'bottom') ) %>%
  ggsave(filename = 'semanticdecision/frequentist_bayesian_plots/plots/semanticdecision_frequentist_bayesian_plot_weaklyinformativepriors_exgaussian.pdf',
         device = cairo_pdf, width = 5.5, height = 6.5, dpi = 900)

# Plot adjusted to fit in the combination of all three plots
plot_semanticdecision_frequentist_bayesian_plot_weaklyinformativepriors_exgaussian =
  frequentist_bayesian_plot(KR_summary_semanticdecision_lmerTest,
                            confint_semanticdecision_lmerTest,
                            semanticdecision_posteriordistributions_weaklyinformativepriors_exgaussian,
                            labels = new_labels, interaction_symbol_x = TRUE,
                            vertical_line_at_x = 0, x_title = 'Effect size (&beta;)', 
                            x_axis_labels = 3, note_frequentist_no_prior = TRUE) +
  ggtitle('Prior *SD* = 0.2') +
  theme(axis.text.y = element_blank())

#####

plot_semanticdecision_frequentist_bayesian_plot_diffusepriors_exgaussian =
  frequentist_bayesian_plot(KR_summary_semanticdecision_lmerTest,
                            confint_semanticdecision_lmerTest,
                            semanticdecision_posteriordistributions_diffusepriors_exgaussian,
                            labels = new_labels, interaction_symbol_x = TRUE,
                            vertical_line_at_x = 0, x_title = 'Effect size (&beta;)', 
                            x_axis_labels = 3, note_frequentist_no_prior = TRUE) +
  ggtitle('Prior *SD* = 0.3') + 
  theme(axis.text.y = element_blank())

#####


# Save combination of the three plots

( plot_semanticdecision_frequentist_bayesian_plot_informativepriors_exgaussian +
    plot_semanticdecision_frequentist_bayesian_plot_weaklyinformativepriors_exgaussian +
    plot_semanticdecision_frequentist_bayesian_plot_diffusepriors_exgaussian +
    
    plot_layout(ncol = 3, guides = 'collect') & theme(legend.position = 'bottom') ) %>%
  
  ggsave(filename = 'semanticdecision/frequentist_bayesian_plots/plots/semanticdecision_frequentist_bayesian_plot_allpriors_exgaussian.pdf',
         device = cairo_pdf, width = 8, height = 7, dpi = 900)


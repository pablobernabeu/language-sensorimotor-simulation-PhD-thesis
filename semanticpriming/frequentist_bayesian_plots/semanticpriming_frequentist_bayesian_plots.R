

# Part of Study 1: Semantic priming

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

KR_summary_semanticpriming_lmerTest = 
  readRDS('semanticpriming/frequentist_analysis/results/KR_summary_semanticpriming_lmerTest.rds')

confint_semanticpriming_lmerTest = 
  readRDS('semanticpriming/frequentist_analysis/results/confint_semanticpriming_lmerTest.rds')

# Below are the default names of the effects
rownames(KR_summary_semanticpriming_lmerTest$coefficients)
rownames(confint_semanticpriming_lmerTest)

# Load Bayesian posterior distributions

semanticpriming_posteriordistributions_informativepriors_exgaussian = 
  readRDS('semanticpriming/bayesian_analysis/results/semanticpriming_posteriordistributions_informativepriors_exgaussian.rds')

semanticpriming_posteriordistributions_weaklyinformativepriors_exgaussian =
  readRDS('semanticpriming/bayesian_analysis/results/semanticpriming_posteriordistributions_weaklyinformativepriors_exgaussian.rds')

semanticpriming_posteriordistributions_diffusepriors_exgaussian = 
  readRDS('semanticpriming/bayesian_analysis/results/semanticpriming_posteriordistributions_diffusepriors_exgaussian.rds')

# Below are the default names of the effects
levels(semanticpriming_posteriordistributions_diffusepriors_exgaussian$data$parameter)


# Reorder the components of interactions in the frequentist results to match 
# with the order present in the Bayesian results.

rownames(KR_summary_semanticpriming_lmerTest$coefficients) =
  rownames(KR_summary_semanticpriming_lmerTest$coefficients) %>%
  str_replace(pattern = 'z_recoded_interstimulus_interval:z_cosine_similarity', 
              replacement = 'z_cosine_similarity:z_recoded_interstimulus_interval') %>%
  str_replace(pattern = 'z_recoded_interstimulus_interval:z_visual_rating_diff', 
              replacement = 'z_visual_rating_diff:z_recoded_interstimulus_interval')

rownames(confint_semanticpriming_lmerTest)  = 
  rownames(confint_semanticpriming_lmerTest) %>%
  str_replace(pattern = 'z_recoded_interstimulus_interval:z_cosine_similarity', 
              replacement = 'z_cosine_similarity:z_recoded_interstimulus_interval') %>%
  str_replace(pattern = 'z_recoded_interstimulus_interval:z_visual_rating_diff', 
              replacement = 'z_visual_rating_diff:z_recoded_interstimulus_interval')


# Create a vector containing the names of the effects. This vector will be passed 
# to the plotting function.

new_labels = 
  
  semanticpriming_posteriordistributions_diffusepriors_exgaussian$data$parameter %>% 
  unique %>%
  
  # Remove the default 'b_' from the beginning of each effect
  str_remove('^b_') %>%
  
  # Put Intercept in parentheses
  str_replace(pattern = 'Intercept', replacement = '(Intercept)') %>%
  
  # First, adjust names of variables (both in main effects and in interactions)
  str_replace(pattern = 'z_target_word_frequency',
              replacement = 'Target-word frequency') %>%
  str_replace(pattern = 'z_target_number_syllables',
              replacement = 'Number of target-word syllables') %>%
  str_replace(pattern = 'z_word_concreteness_diff',
              replacement = 'Word-concreteness difference') %>%
  str_replace(pattern = 'z_cosine_similarity',
              replacement = 'Language-based similarity') %>%
  str_replace(pattern = 'z_visual_rating_diff',
              replacement = 'Visual-strength difference') %>%
  str_replace(pattern = 'z_attentional_control',
              replacement = 'Attentional control') %>%
  str_replace(pattern = 'z_vocabulary_size',
              replacement = 'Vocabulary size') %>%
  str_replace(pattern = 'z_recoded_participant_gender',
              replacement = 'Gender') %>%
  str_replace(pattern = 'z_recoded_interstimulus_interval',
              replacement = 'SOA') %>%
  # Show acronym in main effect of SOA
  str_replace(pattern = '^SOA$',
              replacement = 'Stimulus onset asynchrony (SOA)') %>%
  
  # Second, adjust order of effects in interactions. In the output from the model, 
  # the word-level variables of interest (i.e., 'z_cosine_similarity' and 
  # 'z_visual_rating_diff') sometimes appeared second in their interactions. For 
  # better consistency, the code below moves those word-level variables (with 
  # their new names) to the first position in their interactions. Note that the 
  # order does not affect the results in any way.
  sub('(\\w+.*):(Language-based similarity|Visual-strength difference)', 
      '\\2:\\1', 
      .) %>%
  
  # Replace colons denoting interactions with times symbols
  str_replace(pattern = ':', replacement = ' &times; ')


# Create plots, beginning with the informative-prior model

plot_semanticpriming_frequentist_bayesian_plot_informativepriors_exgaussian =
  frequentist_bayesian_plot(KR_summary_semanticpriming_lmerTest,
                            confint_semanticpriming_lmerTest,
                            semanticpriming_posteriordistributions_informativepriors_exgaussian,
                            labels = new_labels, interaction_symbol_x = TRUE,
                            vertical_line_at_x = 0, x_title = 'Effect size (&beta;)', 
                            x_axis_labels = 3, note_frequentist_no_prior = TRUE) +
  ggtitle('Prior *SD* = 0.1')

#####

# Save single plot
( frequentist_bayesian_plot(KR_summary_semanticpriming_lmerTest,
                            confint_semanticpriming_lmerTest,
                            semanticpriming_posteriordistributions_weaklyinformativepriors_exgaussian,
                            labels = new_labels, interaction_symbol_x = TRUE,
                            vertical_line_at_x = 0, x_title = 'Effect size (&beta;)',
                            legend_ncol = 1) + 
    theme(legend.position = 'bottom') ) %>%
  ggsave(filename = 'semanticpriming/frequentist_bayesian_plots/plots/semanticpriming_frequentist_bayesian_plot_weaklyinformativepriors_exgaussian.pdf',
         device = cairo_pdf, width = 6.5, height = 7.5, dpi = 900)

# Plot adjusted to fit in the combination of all three plots
plot_semanticpriming_frequentist_bayesian_plot_weaklyinformativepriors_exgaussian =
  frequentist_bayesian_plot(KR_summary_semanticpriming_lmerTest,
                            confint_semanticpriming_lmerTest,
                            semanticpriming_posteriordistributions_weaklyinformativepriors_exgaussian,
                            labels = new_labels, interaction_symbol_x = TRUE,
                            vertical_line_at_x = 0, x_title = 'Effect size (&beta;)',
                            x_axis_labels = 3, note_frequentist_no_prior = TRUE) +
  ggtitle('Prior *SD* = 0.2') +
  theme(axis.text.y = element_blank())

#####

plot_semanticpriming_frequentist_bayesian_plot_diffusepriors_exgaussian =
  frequentist_bayesian_plot(KR_summary_semanticpriming_lmerTest,
                            confint_semanticpriming_lmerTest,
                            semanticpriming_posteriordistributions_diffusepriors_exgaussian,
                            labels = new_labels, interaction_symbol_x = TRUE,
                            vertical_line_at_x = 0, x_title = 'Effect size (&beta;)', 
                            x_axis_labels = 3, note_frequentist_no_prior = TRUE) +
  ggtitle('Prior *SD* = 0.3') + 
  theme(axis.text.y = element_blank())

#####


# Save combination of the three plots

( plot_semanticpriming_frequentist_bayesian_plot_informativepriors_exgaussian +
    plot_semanticpriming_frequentist_bayesian_plot_weaklyinformativepriors_exgaussian +
    plot_semanticpriming_frequentist_bayesian_plot_diffusepriors_exgaussian +
    
    plot_layout(ncol = 3, guides = 'collect') & theme(legend.position = 'bottom') ) %>%
  
  ggsave(filename = 'semanticpriming/frequentist_bayesian_plots/plots/semanticpriming_frequentist_bayesian_plot_allpriors_exgaussian.pdf',
         device = cairo_pdf, width = 8, height = 7, dpi = 900)


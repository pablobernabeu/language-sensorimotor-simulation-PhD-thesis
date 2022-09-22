

# Part of Study 1: Semantic priming

# Plots of the posterior predictive checks

# Note. Only the plots are created in this script. The posterior predictive 
# checks themselves were created in the script of each model--e.g., in
# 'semanticpriming/bayesian_analysis/semanticpriming_brms_informativepriors_exgaussian.R'


library(dplyr)
library(ggplot2)
library(patchwork)
library(Cairo)

# Load custom plotting function
source('R_functions/plot_posteriorpredictivecheck.R')

# Load posterior predictive checks

semanticpriming_posteriorpredictivecheck_informativepriors_exgaussian =
  readRDS('semanticpriming/bayesian_analysis/posterior_predictive_checks/results/semanticpriming_posteriorpredictivecheck_informativepriors_exgaussian.rds')

semanticpriming_posteriorpredictivecheck_weaklyinformativepriors_exgaussian =
  readRDS('semanticpriming/bayesian_analysis/posterior_predictive_checks/results/semanticpriming_posteriorpredictivecheck_weaklyinformativepriors_exgaussian.rds')

semanticpriming_posteriorpredictivecheck_diffusepriors_exgaussian =
  readRDS('semanticpriming/bayesian_analysis/posterior_predictive_checks/results/semanticpriming_posteriorpredictivecheck_diffusepriors_exgaussian.rds')


# Create plots, beginning with the informative-prior model

semanticpriming_posteriorpredictivecheck_informativepriors_exgaussian =
  semanticpriming_posteriorpredictivecheck_informativepriors_exgaussian %>%
  plot_posteriorpredictivecheck() + 
  ggtitle('Prior *SD* = 0.1') + xlab('RT (*z*)') + 
  theme(legend.position = 'none')

# Weakly-informative-prior model
semanticpriming_posteriorpredictivecheck_weaklyinformativepriors_exgaussian =
  semanticpriming_posteriorpredictivecheck_weaklyinformativepriors_exgaussian %>%
  plot_posteriorpredictivecheck() + 
  ggtitle('Prior *SD* = 0.2') + xlab('RT (*z*)') + 
  theme(legend.position = 'none', axis.title.y = element_blank())

# Diffuse-prior model
semanticpriming_posteriorpredictivecheck_diffusepriors_exgaussian =
  semanticpriming_posteriorpredictivecheck_diffusepriors_exgaussian %>%
  plot_posteriorpredictivecheck() + 
  ggtitle('Prior *SD* = 0.3') + xlab('RT (*z*)') + 
  theme(axis.title.y = element_blank(), 
        legend.position = c(.75, .7),
        legend.text = element_text(size = 17), 
        plot.margin = margin(12, 0, 14, 12))


# Combine plots

( semanticpriming_posteriorpredictivecheck_informativepriors_exgaussian +
    semanticpriming_posteriorpredictivecheck_weaklyinformativepriors_exgaussian +
    semanticpriming_posteriorpredictivecheck_diffusepriors_exgaussian +
    # plot_annotation(tag_levels = list(c('(a)', '(b)', '(c)'))) + 
    plot_layout(ncol = 3) ) %>%
  
  # Save to disk
  ggsave(filename = 'semanticpriming/bayesian_analysis/posterior_predictive_checks/plots/semanticpriming_posteriorpredictivechecks_allpriors_exgaussian.pdf',
         device = cairo_pdf, width = 10, height = 4, dpi = 900)



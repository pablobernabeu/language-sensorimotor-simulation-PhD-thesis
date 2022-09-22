

# Part of Study 1: Semantic priming

# Combination of plots:
# 1. Interaction between cosine similarity and SOA
# 2. Interaction between visual rating difference and SOA

library(dplyr)
library(ggplot2)
library(patchwork)

# Data set below created in the script 'semanticpriming_data_preparation.R',
# which is stored in the folder 'semanticpriming/data'
semanticpriming = read.csv('semanticpriming/data/final_dataset/semanticpriming.csv')


## Convert interstimulus interval to stimulus onset asynchrony ##

# The stimulus onset asynchrony (SOA) has an alternative formula called the 
# 'interstimulus interval' (ISI). The difference between these is that the 
# ISI does not count the presentation of the prime word (example 
# equivalences between ISI and SOA are available ISI and SOA in Lam et al., 
# 2015, and in Figure 1A of Di Lollo et al., 2004). In the current study, 
# the presentation of the prime word lasts 150 ms. Thus, the 200-ms SOA is 
# equivalent to an ISI of 50 ms, and the 1,200-ms SOA corresponds to an 
# ISI of 1,050 ms (Hutchison et al., 2013). The use of either formula in 
# the analysis would not change our results, as we recoded the levels of 
# the factor as -0.5 and +0.5, and then z-scored those, following the 
# advice of Brauer and Curtin (2018). In our analyses (https://osf.io/ueryq), 
# we used the ISI formula, as it was the one present in the data set of 
# Hutchison et al. (2013; https://www.montana.edu/attmemlab/documents/all%20ldt%20subs_all%20trials3.xlsx). 
# However, we use the SOA formula in this paper as it has been more 
# commonly used in previous papers (e.g., Hutchison et al., 2013; Pecher
# et al., 1998; Petilli et al., 2021; Yap et al., 2017).

# The 'SOA' column is created below by replacing ISI values of 50 with 
# 200 and ISI values of 1050 with 1200.

semanticpriming$SOA = 
  ifelse(semanticpriming$interstimulus_interval == 50, 200, 
         ifelse(semanticpriming$interstimulus_interval == 1050, 1200, 
                semanticpriming$interstimulus_interval))


# Model below created in the script 'semanticpriming_lmerTest.R',
# which is stored in the folder 'semanticpriming/frequentist_analysis'
semanticpriming_lmerTest = 
  readRDS('semanticpriming/frequentist_analysis/results/semanticpriming_lmerTest.rds')

# Load custom function
source('R_functions/alias_interaction_plot.R')

plot1 =
  alias_interaction_plot(
    model = semanticpriming_lmerTest,
    dataset = semanticpriming,
    x = 'z_cosine_similarity',
    fill = 'z_recoded_interstimulus_interval',
    fill_alias = 'SOA',
    x_title = 'Language-based similarity (*z*)',
    y_title = 'Predicted RT (*z*)',
    fill_title = 'SOA (ms)'
  ) + theme(plot.tag.position = c(0, 1), 
            legend.position = c(.9, .82))

plot2 = 
  alias_interaction_plot(
    model = semanticpriming_lmerTest,
    dataset = semanticpriming,
    x = 'z_visual_rating_diff',
    fill = 'z_recoded_interstimulus_interval',
    fill_alias = 'SOA',
    x_title = 'Visual-strength difference (*z*)',
    y_title = 'Predicted RT (*z*)',
    fill_title = 'SOA (ms)'
  ) + theme(plot.tag.position = c(0, 1), 
            legend.position = 'none')

# Combine plots using {patchwork} and save the result to disk
( plot1 + plot2 + 
    plot_annotation(tag_levels = list(c('(a)', '(b)'))) + 
    plot_layout(ncol = 1) ) %>%
  ggsave(filename = 'semanticpriming/frequentist_analysis/plots/semanticpriming-interactions-with-SOA.pdf',
         device = cairo_pdf, width = 6, height = 7, dpi = 900)


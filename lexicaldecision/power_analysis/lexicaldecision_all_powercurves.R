

# Part of Study 3: Lexical decision

# Power analysis

# Since the power curves on the semantic priming data take several weeks to run, they were chunked up in
# order to run them in parallel (see 'individual_scripts' folder). The chunked results are unified below 
# (for background on this method, see https://pablobernabeu.github.io/2021/parallelizing-simr-powercurve).


# install.packages('tidyverse', repos = 'https://www.stats.bris.ac.uk/R/')
# install.packages('patchwork', repos = 'https://www.stats.bris.ac.uk/R/')
# install.packages('Cairo', repos = 'https://www.stats.bris.ac.uk/R/')

library(tidyverse)    # data wrangling, text processing and plots
library(patchwork)    # combine plots
library(Cairo)        # Allows use of special characters such as dashes in plots


# Read in the results from all individual scripts
files_list = 
  list.files(pattern = '*.rds', full.names = TRUE,
             path = 'lexicaldecision/power_analysis/results') %>% 
  map(readRDS)

# Name each file
names(files_list) = 
  list.files(pattern = '*.rds', 
             path = 'lexicaldecision/power_analysis/results') %>% 
  str_remove_all('.rds')

# Load function to consolidate the chunks of each power curve,
# which were run separately and in parallel to save time.
source('R_functions/combine_powercurve_chunks.R')

# Load function to plot power curves
source('R_functions/powercurvePlot.R')

# Below is the title of each power curve (uncomment line to run code)
# for(i in 1: length(files_list)) print(files_list[[i]]['text'] %>% paste)

# The following for-loop adjusts the titles of the power curves in two ways...
for(i in 1 : length(files_list)) {
  files_list[[i]]['text'] = files_list[[i]]['text'] %>% 
    
    # First, adjust names of variables (both in main effects and in interactions)
    str_replace(pattern = 'z_word_frequency',
                replacement = 'Word frequency') %>%
    str_replace(pattern = 'z_visual_rating',
                replacement = 'Visual strength') %>%
    str_replace(pattern = 'z_vocabulary_age',
                replacement = 'Vocabulary age') %>%
    str_replace(pattern = 'z_recoded_participant_gender',
                replacement = 'Gender') %>%
    
    # Second, adjust order of effects in interactions. In the original model output, 
    # the word-level variables (i.e., 'z_word_frequency' and 'z_visual_rating') 
    # sometimes appeared second in their interactions. For better consistency, the 
    # code below moves those word-level variables (with their new names) to the 
    # first position in their interactions. Note that the order does not affect 
    # the results in any way.
    sub("'(\\w+.*):(Word frequency|Visual strength)'", '\\2:\\1', .)
}

# Furthermore, the 'powercurvePlot' function used below internally removes the 
# text 'Power for predictor' from the output of the 'simr' package, leaving 
# only the name of the predictor in the title of each power curve.


# Power curve 1

# Table
# combine_powercurve_chunks(files_list, powercurve_number = 1)

# Plot
powercurve1_plot = 
  combine_powercurve_chunks(files_list, powercurve_number = 1) %>% 
  powercurvePlot(number_x_axis_levels = 10)


# Power curve 2

# Table
# combine_powercurve_chunks(files_list, powercurve_number = 2)

# Plot
powercurve2_plot = 
  combine_powercurve_chunks(files_list, powercurve_number = 2) %>% 
  powercurvePlot(number_x_axis_levels = 10)


# Power curve 3

# Table
# combine_powercurve_chunks(files_list, powercurve_number = 3)

# Plot
powercurve3_plot = 
  combine_powercurve_chunks(files_list, powercurve_number = 3) %>% 
  powercurvePlot(number_x_axis_levels = 10)


# Power curve 4

# Table
# combine_powercurve_chunks(files_list, powercurve_number = 4)

# Plot
powercurve4_plot = 
  combine_powercurve_chunks(files_list, powercurve_number = 4) %>% 
  powercurvePlot(number_x_axis_levels = 10)


# Power curve 5

# Table
# combine_powercurve_chunks(files_list, powercurve_number = 5)

# Plot
powercurve5_plot = 
  combine_powercurve_chunks(files_list, powercurve_number = 5) %>% 
  powercurvePlot(number_x_axis_levels = 10)


# Power curve 6

# Table
# combine_powercurve_chunks(files_list, powercurve_number = 6)

# Plot
powercurve6_plot = 
  combine_powercurve_chunks(files_list, powercurve_number = 6) %>% 
  powercurvePlot(number_x_axis_levels = 10)


# Power curve 7

# Table
# combine_powercurve_chunks(files_list, powercurve_number = 7)

# Plot
powercurve7_plot = 
  combine_powercurve_chunks(files_list, powercurve_number = 7) %>% 
  powercurvePlot(number_x_axis_levels = 10)


# Combine plots

lexicaldecision_powercurve_plots_1_2_3 = 
  powercurve1_plot + powercurve2_plot + 
  powercurve3_plot + plot_layout(nrow = 2)

lexicaldecision_powercurve_plots_4_5_6_7 = 
  powercurve4_plot + powercurve5_plot + 
  powercurve6_plot + powercurve7_plot

# Free up workspace
rm(list = c('powercurve1_plot', 'powercurve2_plot', 'powercurve3_plot', 
            'powercurve4_plot', 'powercurve5_plot', 'powercurve6_plot', 
            'powercurve7_plot'))

# Remove redundant labels to fit with the multiplot arrangement
# and adjust margins

lexicaldecision_powercurve_plots_1_2_3[[1]] = 
  lexicaldecision_powercurve_plots_1_2_3[[1]] + 
  theme(axis.title.x = element_blank(),
        plot.margin = margin(8, 2, 0, 0))

lexicaldecision_powercurve_plots_1_2_3[[2]] = 
  lexicaldecision_powercurve_plots_1_2_3[[2]] + 
  theme(axis.title.y = element_blank(),
        plot.margin = margin(8, 0, 0, 0))

lexicaldecision_powercurve_plots_1_2_3[[3]] = 
  lexicaldecision_powercurve_plots_1_2_3[[3]] + 
  theme(plot.margin = margin(0, 2, 11, 0))

lexicaldecision_powercurve_plots_4_5_6_7[[1]] = 
  lexicaldecision_powercurve_plots_4_5_6_7[[1]] + 
  theme(axis.title.x = element_blank(),
        plot.margin = margin(8, 2, 15, 0))

lexicaldecision_powercurve_plots_4_5_6_7[[2]] = 
  lexicaldecision_powercurve_plots_4_5_6_7[[2]] + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(8, 0, 15, 0))

lexicaldecision_powercurve_plots_4_5_6_7[[3]] = 
  lexicaldecision_powercurve_plots_4_5_6_7[[3]] + 
  theme(plot.margin = margin(0, 2, 11, 0))

lexicaldecision_powercurve_plots_4_5_6_7[[4]] = 
  lexicaldecision_powercurve_plots_4_5_6_7[[4]] + 
  theme(axis.title.y = element_blank(),
        plot.margin = margin(0, 0, 11, 0))


# Save

ggsave('lexicaldecision/power_analysis/plots/lexicaldecision_powercurve_plots_1_2_3.pdf', 
       plot = lexicaldecision_powercurve_plots_1_2_3, device = cairo_pdf, 
       width = 7, height = 6.4, units = 'in', dpi = 800)

ggsave('lexicaldecision/power_analysis/plots/lexicaldecision_powercurve_plots_4_5_6_7.pdf', 
       plot = lexicaldecision_powercurve_plots_4_5_6_7, device = cairo_pdf, 
       width = 7, height = 6.4, units = 'in', dpi = 800)


# Free up workspace
# rm(list = c('lexicaldecision_powercurve_plots_1_2_3', 
#             'lexicaldecision_powercurve_plots_4_5_6_7'))



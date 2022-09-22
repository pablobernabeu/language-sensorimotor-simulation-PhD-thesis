

# Function for plotting interaction effect by splitting one of the variables 
# (namely, the one passed to the `fill` argument) into ten parts, known as 
# deciles. Where there's a sufficient amount of data, all parts will have 
# the same number of data points.
# The function draws on sjPlot::plot_model. Note that all arguments except 
# `model` and `legend_ncol` must be enclosed in quotation marks. 

# Usage example:

# deciles_interaction_plot(
#   model = semanticdecision_lmerTest,
#   x = 'z_word_cooccurrence',
#   fill = 'z_vocabulary_size',
#   fill_nesting_factor = 'Participant',
#   x_title = "Word co-occurrence (*z*)",
#   y_title = 'Predicted RT (*z*)',
#   fill_title = 'Vocabulary size<br>(*z*, deciles)',
#   legend_ncol = 2
# )

# Note: should you wish to verify the accuracy of these involved plots, 
# you could use sjPlot::plot_model() to produce simpler versions.

#################################


deciles_interaction_plot = 
  
  function(model, x, fill, fill_nesting_factor = NULL, x_title = NULL,
           y_title = NULL, fill_title = NULL, legend_ncol = 1) {
    
    require(dplyr)
    require(ggplot2)
    require(sjPlot)
    require(RColorBrewer)
    require(ggtext)
    require(Cairo)
    
    # Save decile values
    deciles = quantile(model@frame[,fill], prob = seq(0, 1, length = 11), 
                       names = FALSE)
    
    # Create fill argument name, to be passed into the `terms` argument of 
    # the plot. This name includes a [list] of the values to be shown.
    fill_name = paste0(fill, ' [', 
                       deciles[1], ', ', 
                       deciles[2], ', ', 
                       deciles[3], ', ',
                       deciles[4], ', ',
                       deciles[5], ', ', 
                       deciles[6], ', ', 
                       deciles[7], ', ', 
                       deciles[8], ', ', 
                       deciles[9], ', ', 
                       deciles[10], ', ', 
                       deciles[11], 
                       ']')
    
    # Wrap number-processing functions into a single one
    process_labels = function(x) {
      # First, round to two decimal places 
      # while keeping any trailing zeros
      sprintf('%.2f', x) %>% 
        # Now, remove minus sign from any -0.00 
        sub('-0.00', '0.00', .)
    }
    
    # If `fill_nesting_factor` not supplied, create basic legend labels
    if(is.null(fill_nesting_factor)) {
      fill_labels = c(
        process_labels(deciles[1]),
        process_labels(deciles[2]),
        process_labels(deciles[3]),
        process_labels(deciles[4]),
        process_labels(deciles[5]),
        process_labels(deciles[6]),
        process_labels(deciles[7]),
        process_labels(deciles[8]),
        process_labels(deciles[9]),
        process_labels(deciles[10]),
        process_labels(deciles[11])
      )
      
      # Else, if `fill_nesting_factor` was supplied, create legend labels 
      # that include sample size of each decile section at the end
    } else {
      
      n_decile_1 = model@frame[model@frame[,fill] >= deciles[1] & 
                                 model@frame[,fill] < deciles[2], ] %>% 
        select(all_of(fill_nesting_factor)) %>% unique %>% nrow
      
      n_decile_2 = model@frame[model@frame[,fill] >= deciles[2] & 
                                 model@frame[,fill] < deciles[3], ] %>% 
        select(all_of(fill_nesting_factor)) %>% unique %>% nrow
      
      n_decile_3 = model@frame[model@frame[,fill] >= deciles[3] & 
                                 model@frame[,fill] < deciles[4], ] %>% 
        select(all_of(fill_nesting_factor)) %>% unique %>% nrow
      
      n_decile_4 = model@frame[model@frame[,fill] >= deciles[4] & 
                                 model@frame[,fill] < deciles[5], ] %>% 
        select(all_of(fill_nesting_factor)) %>% unique %>% nrow
      
      n_decile_5 = model@frame[model@frame[,fill] >= deciles[5] & 
                                 model@frame[,fill] < deciles[6], ] %>% 
        select(all_of(fill_nesting_factor)) %>% unique %>% nrow
      
      n_decile_6 = model@frame[model@frame[,fill] >= deciles[6] & 
                                 model@frame[,fill] < deciles[7], ] %>% 
        select(all_of(fill_nesting_factor)) %>% unique %>% nrow
      
      n_decile_7 = model@frame[model@frame[,fill] >= deciles[7] & 
                                 model@frame[,fill] < deciles[8], ] %>% 
        select(all_of(fill_nesting_factor)) %>% unique %>% nrow
      
      n_decile_8 = model@frame[model@frame[,fill] >= deciles[8] & 
                                 model@frame[,fill] < deciles[9], ] %>% 
        select(all_of(fill_nesting_factor)) %>% unique %>% nrow
      
      n_decile_9 = model@frame[model@frame[,fill] >= deciles[9] & 
                                 model@frame[,fill] < deciles[10], ] %>% 
        select(all_of(fill_nesting_factor)) %>% unique %>% nrow
      
      n_decile_10 = model@frame[model@frame[,fill] >= deciles[10] & 
                                  model@frame[,fill] <= deciles[11], ] %>% 
        select(all_of(fill_nesting_factor)) %>% unique %>% nrow
      
      fill_labels = c(
        paste0(process_labels(deciles[1]), ' (*n* = ', n_decile_1, ')'),
        paste0(process_labels(deciles[2]), ' (*n* = ', n_decile_2, ')'),
        paste0(process_labels(deciles[3]), ' (*n* = ', n_decile_3, ')'),
        paste0(process_labels(deciles[4]), ' (*n* = ', n_decile_4, ')'),
        paste0(process_labels(deciles[5]), ' (*n* = ', n_decile_5, ')'),
        paste0(process_labels(deciles[6]), ' (*n* = ', n_decile_6, ')'),
        paste0(process_labels(deciles[7]), ' (*n* = ', n_decile_7, ')'),
        paste0(process_labels(deciles[8]), ' (*n* = ', n_decile_8, ')'),
        paste0(process_labels(deciles[9]), ' (*n* = ', n_decile_9, ')'),
        paste0(process_labels(deciles[10]), ' (*n* = ', n_decile_10, ')'),
        process_labels(deciles[11])
      )
    }
    
    # Create X-axis title if none supplied by user
    if(is.null(x_title)) x_title = x
    
    # Create Y-axis title if none supplied by user
    if(is.null(y_title)) {
      y_title = paste('Predicted values of', 
                      colnames(model@frame)[1]) # dependent variable
    }
    
    # Create fill-legend title if none supplied by user
    if(is.null(fill_title)) fill_title = paste0(fill, '<br>(deciles)')
    
    # Plot
    
    plot_model(model, type = 'pred', terms = c(x, fill_name), ci.lvl = .95) +
      
      geom_point(show.legend = FALSE) + 
      scale_x_continuous(expand = expansion(mult = c(.01, .01))) + 
      scale_y_continuous(expand = expansion(mult = c(.01, .01))) + 
      
      guides(color = 'none', 
             fill = guide_legend(title = fill_title, ncol = legend_ncol,
                                 # In each key of the legend, replace the 
                                 # default line with a full square.
                                 override.aes = 
                                   list(linetype = rep(0, n_distinct(fill)), 
                                        alpha = 1), 
                                 # Reverse legend labels to have them in 
                                 # ascending order, with the 0th decile
                                 # placed at the bottom.
                                 reverse = TRUE)) +
      
      # Use colour scale ranging from red to blue in both the `colour` and
      # the `fill` parts of the plot. Take legend labels from `fill_labels`.
      scale_colour_brewer(palette = 'RdYlBu', aesthetics = c('colour', 'fill'),
                          labels = fill_labels, 
                          # Assign red to positive values 
                          # and blue to negative ones
                          direction = -1) +
      
      xlab(x_title) + ylab(y_title) +
      theme(plot.title = element_blank(), panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), panel.background = element_blank(), 
            axis.title.x = ggtext::element_markdown(size = 12.5, 
                                                    margin = margin(t = 6)),
            axis.title.y = ggtext::element_markdown(size = 12.5, 
                                                    margin = margin(r = 6)),
            axis.text = element_text(size = 11), 
            axis.line = element_line(colour = 'black'), 
            legend.title = 
              ggtext::element_markdown(size = 12.5, hjust = 0.5, vjust = 0.5, 
                                       margin = margin(b = 5), lineheight = 1.4), 
            legend.title.align = 0.5, 
            legend.text = ggtext::element_markdown(
              # Adjust size of text depending on number of levels in fill_labels
              size = ifelse(length(fill_labels) < 4, 11, 10.5), 
              vjust = .5), 
            # Adjust size of legend keys depending on number of levels in fill_labels
            legend.key.size = unit(ifelse(length(fill_labels) < 4, 0.7, 0.6), 'cm'), 
            legend.background = element_rect(colour = 'grey70', 
                                             fill = 'transparent'),
            legend.margin = margin(7, 7, 7, 7),
            plot.margin = margin(10, 6, 10, 10))
    
  }


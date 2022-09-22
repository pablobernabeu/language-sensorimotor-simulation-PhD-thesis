

# Function for plotting interaction effect by presenting one of the variables 
# (namely, the variable passed to the `fill` argument) as an alias variable 
# (`fill_alias`). The main purpose is facilitating the visualisation of
# interactions involving a categorical factor that was z-scored in the 
# statistical model. The function draws on `sjPlot::plot_model()`. Note that 
# all arguments except `model`, `dataset` and `legend_ncol` must be enclosed 
# in quotation marks. 

# Usage example:

# alias_interaction_plot(
#   model = semanticpriming_lmerTest,
#   dataset = semanticpriming,
#   x = 'z_cosine_similarity',
#   fill = 'z_recoded_participant_gender',
#   fill_alias = 'participant_gender',
#   fill_nesting_factor = 'Participant',
#   x_title = 'Language-based similarity (*z*)',
#   y_title = 'Predicted target-word RT (*z*)',
#   fill_title = 'Gender'
# )

# Note: should you wish to verify the accuracy of these involved plots, 
# you could use sjPlot::plot_model() to produce simpler versions.

#################################


alias_interaction_plot = 
  
  function(model, dataset, x, fill, fill_alias, fill_nesting_factor = NULL, 
           x_title = NULL, y_title = NULL, fill_title = NULL, legend_ncol = 1) {
    
    require(rlang)  # needed for `!!parse_expr()`
    require(dplyr)
    require(ggplot2)
    require(sjPlot)
    require(RColorBrewer)
    require(ggtext)
    require(Cairo)
    
    # Import fill_alias into the model by finding the match with
    # the `fill` in the model and the `fill` in the data set.
    model@frame[,fill_alias] = 
      dataset[,fill_alias][match(model@frame[,fill], dataset[,fill])]
    
    # Save model in a new data frame, which will only be used 
    # to create the fill_labels.
    model_df = as.data.frame(model@frame)
    
    # If appropriate, try appending sample size of 
    # `fill_nesting_factor` to `fill_alias`
    if(!is.null(fill_nesting_factor)) {
      
      # Proceed only if the `fill` and the `fill_alias` variables 
      # contain the same number of unique values.
      if(identical(n_distinct(model_df[,fill]), 
                   n_distinct(model_df[,fill_alias]))) {
        
        # Temporarily rename fill_alias as 'temp' 
        model_df = model_df %>% rename(temp = !!parse_expr(fill_alias))
        
        # Append the sample size of fill_nesting_factor to fill_alias
        model_df = model_df %>% 
          group_by(temp) %>%
          mutate(temp = paste0(temp, ' (*n* = ',
                               n_distinct(!!parse_expr(fill_nesting_factor)), 
                               ')'))
        
        # Resolve temporary name of fill_alias to its true name
        colnames(model_df)[which(colnames(model_df) == 'temp')] = fill_alias
        
        # Format as a data frame
        model_df = as.data.frame(model_df)
        
        # If the `fill` and the `fill_alias` variables do not contain the
        # same number of unique values, warn that`fill_nesting_factor` 
        # will be ignored.
      } else {
        message(
          paste('The argument `fill_nesting_factor` has been ignored, as it requires', 
                'that the `fill` variable contain the same number of unique values', 
                'as the `fill_alias` variable.',
                sep = '\n')
        )
      }
    }
    
    # Format fill_alias as a factor
    model_df[,fill_alias] = as.factor(model_df[,fill_alias])
    
    # Format final model depending on the relationship between `fill` and 
    # `fill_alias`. If these variables contain the same number of unique 
    # values, rename `fill_alias` as `fill`, and order the levels.
    if(identical(n_distinct(model@frame[,fill]), 
                 n_distinct(model@frame[,fill_alias]))) {
      
      # Order fill levels by their true order (important to avoid major error)
      fill_labels = c()
      for(i in 1 : length(levels(model_df[,fill_alias]))) {
        fill_labels[i] = model_df[model_df[,fill] == 
                                    levels(as.factor(model_df[,fill]))[i], 
                                  fill_alias] %>% unique %>% as.character
      }
      
      # In the main model, rename fill_alias as fill
      colnames(model@frame)[which(colnames(model@frame) == fill_alias)] = fill
      
      # Else, if `fill` and `fill_alias` do not contain the same number of 
      # unique values, assign values of `fill_alias` to `fill`, format it 
      # as a factor, and create fill labels directly from its levels.
    } else {
      model@frame[,fill] = as.factor(model@frame[,fill_alias])
      fill_labels = levels(model@frame[,fill])
    }
    
    # Create X-axis title if none supplied by user
    if(is.null(x_title)) x_title = x
    
    # Create Y-axis title if none supplied by user
    if(is.null(y_title)) {
      y_title = paste('Predicted values of', 
                      colnames(model@frame)[1]) # dependent variable
    }
    
    # Create fill-legend title if none supplied by user
    if(is.null(fill_title)) fill_title = fill
    
    # Plot
    
    plot_model(model, type = 'pred', terms = c(x, fill), ci.lvl = .95) +
      
      geom_point(show.legend = FALSE) + 
      scale_x_continuous(expand = expansion(mult = c(.01, .01))) + 
      scale_y_continuous(expand = expansion(mult = c(.01, .01))) + 
      
      guides(color = 'none',
             fill = guide_legend(title = fill_title, ncol = legend_ncol,
                                 # In each key of the legend, replace the 
                                 # default line with a full square.
                                 override.aes = 
                                   list(linetype = rep(0, n_distinct(fill)), 
                                        alpha = 1))) +
      
      # Take legend labels from `fill_labels`
      scale_colour_discrete(aesthetics = c('colour', 'fill'), 
                            labels = fill_labels, 
                            # With binary factors, assign blue colour 
                            # to first level and red to second level.
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



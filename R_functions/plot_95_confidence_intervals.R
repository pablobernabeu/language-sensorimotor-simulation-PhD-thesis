

# Plot effects in 'lmerTest' models including 95% confidence intervals. Two arguments 
# are required for this function: a summary of a 'lmerTest' model and confidence 
# intervals from lme4::confint.merMod(). The names of the predictors must be 
# identical in both these objects.


plot_95_confidence_intervals =
  
  function( model_summary, confidence_intervals, show_intercept = TRUE, 
            select_effects = NULL, order_effects = NULL, 
            x_title = 'Estimate', axis_text_size = 9, 
            
            # If interaction_symbol_x = TRUE, replace double colons with times 
            # symbols followed by line breaks and indentation. Then, replace 
            # single colons with times symbols.
            interaction_symbol_x = FALSE,
            
            # Vertical line at specified value of X axis
            vertical_line_at_x = NULL ) {
    
    require(dplyr)
    require(forcats)
    require(ggplot2)
    require(stringr)
    require(ggtext)
    
    # Create data frame
    model_summary = data.frame(Effect = rownames(model_summary$coefficients),
                               model_summary$coefficients, row.names = NULL)
    
    # Add 95% confidence intervals
    
    confints = data.frame(Effect = rownames(confidence_intervals), 
                          CI_2.5 = confidence_intervals[,'2.5 %'], 
                          CI_97.5 = confidence_intervals[,'97.5 %'], 
                          row.names = NULL)
    
    model_summary = left_join(model_summary, confints, by = 'Effect')
    
    # If show_intercept = FALSE, remove it
    if(isFALSE(show_intercept)) {
      model_summary = model_summary %>% filter(!Effect == '(Intercept)')
    }
    
    # If select_effects was supplied, apply it and order effects accordingly
    if(!is.null(select_effects)) {
      model_summary = model_summary %>% filter(Effect %in% select_effects) %>%
        arrange(factor(Effect, levels = select_effects))
    }
    
    # If order_effects was supplied, apply order
    if(!is.null(order_effects)) {
      model_summary = model_summary %>%
        arrange(factor(Effect, levels = order_effects))
    }
    
    # If interaction_symbol_x = TRUE, replace double colons with times 
    # symbols followed by line breaks and indentation. Then, replace 
    # single colons with times symbols.
    if(interaction_symbol_x) {
      model_summary$Effect = model_summary$Effect %>% 
        gsub('::', ' &times; <br> &nbsp;&nbsp;&nbsp;&nbsp;', .) %>%
        gsub(':', ' &times; ', .)
    }
    
    # Set current order of effects to prevent alphabetical order
    model_summary$Effect = 
      factor(model_summary$Effect, levels = model_summary$Effect)
    
    # Plot
    
    model_summary %>%
      ggplot(aes(x = Estimate, 
                 # Override default, bottom-up order on Y-axis
                 y = fct_rev(Effect))) + 
      
      # Add vertical line
      geom_vline(xintercept = vertical_line_at_x, col = 'grey60') +
      
      # Add confidence intervals from frequentist model
      geom_errorbarh(aes(xmin = CI_2.5, xmax = CI_97.5), 
                     height = 0.3, colour = 'grey20') +
      
      # Add frequentist estimate
      geom_point(aes(x = Estimate), colour = 'black', size = 1) +
      
      scale_y_discrete(expand = c(0.033, 0)) +
      
      # X-axis title
      xlab(x_title) +
      
      theme_minimal() +
      theme(axis.title.x = ggtext::element_markdown(size = axis_text_size, 
                                                    margin = 
                                                      margin(t = axis_text_size * 0.8)), 
            axis.text.x = element_text(size = axis_text_size * 0.9, 
                                       margin = margin(t = axis_text_size * 0.5)), 
            axis.title.y = element_blank(), 
            axis.text.y = ggtext::element_markdown(size = axis_text_size),
            axis.ticks.x = element_line(),
            panel.grid.major.y = element_line(colour = 'grey85'))
    
  }


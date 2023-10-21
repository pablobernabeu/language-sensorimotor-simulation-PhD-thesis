

# Presenting frequentist and Bayesian estimates in the same plot. For this 
# purpose, the frequentist results are merged into a plot from 
# brms::mcmc_plot(), Three arguments are required for this function: a 
# summary of a 'lmerTest' model, confidence intervals from 
# lme4::confint.merMod(), and a plot from brms::mcmc_plot(). The function 
# is equipped to accept the slight differences between the names of the 
# predictors in the frequentist results and in the Bayesian results.


frequentist_bayesian_plot =
  
  function( frequentist_model_summary, confidence_intervals, mcmc_plot, 
            labels = NULL, font_size = 10, x_title = 'Estimate', 
            x_axis_labels = 6, # <-- approximate number of labels in X axis
            
            # Number of columns into which the legend labels (i.e., 
            # 'Frequentist analysis', 'Bayesian analysis') should 
            # be distributed.
            legend_ncol = 2,
            
            # If note_frequentist_no_prior = TRUE, '(no prior)' is appended to the
            # label 'Frequentist analysis' in the legend. This is useful when
            # the plot is titled with the prior, to clarify that the prior 
            # does not concern the frequentist analysis. 
            note_frequentist_no_prior = FALSE,
            
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
    
    # Format frequentist results to match the format of mcmc_plot
    
    # Firstly, remove parentheses from the intercept and add 'b_' 
    # to the beginning of every predictor
    
    rownames(frequentist_model_summary$coefficients) =
      rownames(frequentist_model_summary$coefficients) %>%
      str_replace(pattern = '\\(Intercept\\)', replacement = 'Intercept') %>%
      str_replace(pattern = '^', replacement = 'b_')
    
    rownames(confidence_intervals) = 
      rownames(confidence_intervals) %>%
      str_replace(pattern = '\\(Intercept\\)', replacement = 'Intercept') %>%
      str_replace(pattern = '^', replacement = 'b_')
    
    # Next, create 'parameters' column in the frequentist results
    
    frequentist_model_summary$coefficients = 
      frequentist_model_summary$coefficients %>% as.data.frame() %>%
      mutate(parameter = rownames(frequentist_model_summary$coefficients)) %>%
      rename(frequentist_estimate = Estimate) %>%
      data.frame(row.names = NULL)
    
    confidence_intervals = 
      confidence_intervals %>% as.data.frame() %>%
      mutate(parameter = rownames(confidence_intervals)) %>%
      rename(CI_2.5 = '2.5 %', CI_97.5 = '97.5 %') %>%
      data.frame(row.names = NULL)
    
    # Merge both frequentist objects into mcmc_plot
    
    mcmc_plot$data =
      list(mcmc_plot$data,
           frequentist_model_summary$coefficients %>%
             select(parameter, frequentist_estimate),
           confidence_intervals) %>%
      purrr::reduce(full_join, by = 'parameter')
    
    # If labels supplied and interaction_symbol_x = TRUE, replace double 
    # colons with times symbols followed by line breaks and indentation. 
    # Then, replace single colons with times symbols.
    
    if(!is.null(labels) & isTRUE(interaction_symbol_x)) {
      labels = labels %>% 
        gsub('::', ' &times; <br> &nbsp;&nbsp;&nbsp;&nbsp;', .) %>%
        gsub(':', ' &times; ', .)
    }
    
    # Set current order of effects to prevent alphabetical order
    labels = factor(labels, levels = labels)
    
    # Plot
    
    plot = mcmc_plot +
      
      # Add vertical line (on top of default line from mcmc_plot)
      geom_vline(xintercept = vertical_line_at_x, col = 'grey60') +
      
      # Add confidence intervals from frequentist model
      geom_errorbarh(aes(xmin = CI_2.5, xmax = CI_97.5), 
                     height = 0.12, position = position_nudge(y = 0.09), 
                     colour = '#ff7474ff') +
      
      # Add frequentist estimate
      geom_point(aes(x = frequentist_estimate), 
                 position = position_nudge(y = 0.09), 
                 colour = 'red', size = 0.8) +
      
      scale_x_continuous(n.breaks = x_axis_labels, 
                         limits = c(min(mcmc_plot$data$CI_2.5, mcmc_plot$data$x), 
                                    max(mcmc_plot$data$CI_97.5, mcmc_plot$data$x))) +
      
      scale_y_discrete(expand = c(0.023, 0.033)) +
      
      # X-axis title
      xlab(x_title) +
      
      theme_minimal() +
      theme(text = element_text(family = 'Arial'),  # <-- prevent R crashing (see https://discourse.mc-stan.org/t/error-code-when-using-pp-check-with-brms/27419/4) 
            plot.title = element_markdown(colour = '#005b96', hjust = 0.5, vjust = 1,
                                          size = font_size * 1.2),
            axis.title.x = ggtext::element_markdown(size = font_size, 
                                                    margin = margin(t = font_size * 0.8)), 
            axis.text.x = element_text(size = font_size * 0.9, 
                                       margin = margin(t = font_size * 0.4)), 
            axis.title.y = element_blank(), 
            # Use 'vjust' used to place each Y axis label at the foot of its respective segment
            axis.text.y = ggtext::element_markdown(size = font_size, vjust = 0),  
            axis.ticks.x = element_line(), 
            # Parameters are adjusted based on number of columns in legend using ifelse()
            legend.text = element_text(size = font_size,
                                       lineheight = ifelse(legend_ncol == 1, 0.3, 1.2), 
                                       margin = margin(r = ifelse(legend_ncol == 1, 2, 4))),
            legend.key.height = unit(ifelse(legend_ncol == 1, 15, 21), 'pt'),
            legend.background = element_rect(colour = 'grey70', fill = 'white'),
            panel.grid.major.y = element_line(colour = 'grey90'),
            plot.margin = margin(9, 4, 14, 12))
    
    # Manual legend for frequentist analysis (red) and Bayesian analysis (blue),
    # subject to the presence of 'note_frequentist_no_prior'.
    
    if(note_frequentist_no_prior) {
      plot = plot +
        geom_line(aes(colour = factor(parameter)), size = 0) +  # <-- placeholder to allow manual scale
        scale_colour_manual(values = c('Frequentist analysis\n(no prior)' = 'red', 
                                       'Bayesian analysis' = '#005b96'),  # <-- fancy blue
                            guide = guide_legend(title = NULL, 
                                                 override.aes = list(size = 5), 
                                                 ncol = legend_ncol)) 
    } else{
      plot = plot +
        geom_line(aes(colour = factor(parameter)), size = 0) +  # <-- placeholder to allow manual scale
        scale_colour_manual(values = c('Frequentist analysis' = 'red', 
                                       'Bayesian analysis' = '#005b96'),  # <-- fancy blue
                            guide = guide_legend(title = NULL, 
                                                 override.aes = list(size = 4), 
                                                 ncol = legend_ncol)) 
    }
    
    # If labels supplied, pass them to 'scale_y_discrete'. This is necessary 
    # because modifying the 'parameter' column directly creates errors in 
    # the plot.
    
    if(!is.null(labels)) {
      plot = plot +
        # Apply modified labels, in a reverse order to match correct order pre-established 
        # by the 'mcmc_plot' function that was used to create the original plots
        # (see https://discourse.mc-stan.org/t/manually-changing-y-axis-tick-labels-in-brms-mcmc-plot/19727/5)
        scale_y_discrete(labels = rev(labels), limits = rev, 
                         # Padding at the borders
                         expand = c(0.023, 0.033))
    }
    
    # Output
    plot
    
  }


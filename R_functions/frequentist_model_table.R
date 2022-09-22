

# Function used in the manuscript to present summaries from 'lmerTest' models 
# in APA-formatted tables. The only obligatory argument to be supplied is a 
# summary of a 'lmerTest' model.

frequentist_model_table = 
  
  function(model_summary, confidence_intervals = NULL, show_intercept = TRUE, 
           select_effects = NULL, order_effects = NULL, format = NULL, 
           
           # If interaction_symbol_x = TRUE, replace double colons with 
           # times symbols followed by line breaks and indentation. 
           # Then, replace single colons with times symbols.
           interaction_symbol_x = FALSE,
           
           caption = 'Summary of the lmerTest model.') {
    
    require(dplyr)
    require(knitr)
    require(tibble)
    require(stringr)
    require(lmerTest)
    require(kableExtra)
    
    # Create data frame
    model_summary = 
      data.frame(Effect = rownames(summary(model_summary)$coefficients),
                 summary(model_summary)$coefficients, row.names = NULL)
    
    # Add 95% confidence intervals if supplied by user. 
    # They can be computed using lme4::confint()
    
    if(!is.null(confidence_intervals)) {
      confints = data.frame(Effect = rownames(confidence_intervals), 
                            CI_2.5 = confidence_intervals[,'2.5 %'], 
                            CI_97.5 = confidence_intervals[,'97.5 %'], 
                            row.names = NULL)
      
      confints$CI_2.5 = confints$CI_2.5 %>% 
        # Round off and keep trailing zeros
        sprintf('%.2f', .) %>% 
        # Remove minus sign from pure zeros
        sub('-0.00', '0.00', .)
      
      confints$CI_97.5 = confints$CI_97.5 %>% 
        # Round off and keep trailing zeros
        sprintf('%.2f', .) %>% 
        # Remove minus sign from pure zeros
        sub('-0.00', '0.00', .)
      
      model_summary = left_join(model_summary, confints, by = 'Effect')
      
      # Show lower and upper bounds alongside
      model_summary$CI_95 = paste0('[', model_summary$CI_2.5, ', ', 
                                   model_summary$CI_97.5, ']')
    }
    
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
    
    # Edit column names
    model_summary = model_summary %>%
      rename('SE' = 'Std..Error', 'p' = 'Pr...t..')
    
    # Round other values
    
    model_summary$Estimate = model_summary$Estimate %>% as.numeric %>% 
      # Round off and keep trailing zeros
      sprintf('%.2f', .) %>% 
      # Remove minus sign from pure zeros
      sub('-0.00', '0.00', .)
    
    model_summary$SE = model_summary$SE %>% as.numeric %>% 
      # Round off and keep trailing zeros
      sprintf('%.2f', .)
    
    model_summary$t.value = model_summary$t.value %>% as.numeric %>% 
      # Round off and keep trailing zeros
      sprintf('%.2f', .)
    
    model_summary$df = model_summary$df %>% as.numeric %>% 
      # Round off and keep trailing zeros
      sprintf('%.2f', .)
    
    # Right-align all columns after first one
    if(is.null(confidence_intervals)) {
      align = c('l', 'r', 'r', 'r', 'r')
    } else align = c('l', 'r', 'r', 'r', 'r', 'r')
    
    # Establish latex or HTML format: if no format supplied, 
    # try to obtain it from knitr, or apply HTML
    if(missing(format) || is.null(format)) {
      if(knitr::is_latex_output()) {
        format = 'latex'
      } else format = 'html'
    }
    
    # HTML format
    if(format == 'html') {
      
      # Format p values following APA format
      model_summary$p = model_summary$p %>% sprintf('%.3f', .) %>%
        gsub('^0.', '.', .) %>% gsub('^.000$', '&lt;.001', .)
      
      # If interaction_symbol_x = TRUE, replace double colons with times 
      # symbols followed by line breaks and indentation. Then, replace 
      # single colons with times symbols.
      if(interaction_symbol_x) {
        model_summary$Effect = model_summary$Effect %>% 
          gsub('::', ' &times; <br> &nbsp;&nbsp;', .) %>%
          gsub(':', ' &times; ', .)
      }
      
      # Select final variables to be shown, depending on whether 
      # confidence intervals available, and format names.
      if(is.null(confidence_intervals)) {
        model_summary = model_summary[, c('Effect', 'Estimate', 
                                          'SE', 't.value', 'p')] %>%
          rename('&beta;' = 'Estimate', '<i>SE</i>' = 'SE', 
                 '<i>t</i>' = 't.value', '<i>p</i>' = 'p')
      } else {
        model_summary = model_summary[, c('Effect', 'Estimate', 'SE', 
                                          'CI_95', 't.value', 'p')] %>%
          rename('&beta;' = 'Estimate', '<i>SE</i>' = 'SE', '95% CI' = 'CI_95', 
                 '<i>t</i>' = 't.value', '<i>p</i>' = 'p')
      }
      
      # Output table
      model_summary %>% 
        
        # Remove header of first column
        rename(' ' = 'Effect') %>%
        
        # Present table
        kbl(digits = 2, booktabs = TRUE, escape = FALSE, align = align,
            
            # Caption of the table (default unless specified)
            caption = caption, 
            
            # Disable occasional line gap (https://stackoverflow.com/a/49018919/7050882)
            linesep = '') %>%
        
        # Apply nice kableExtra format
        kable_styling() %>%
        
        # Center-align header row
        row_spec(0, align = 'c')
      
      # LaTeX format
    } else {
      
      # Format p values following APA format
      model_summary$p = model_summary$p %>% sprintf('%.3f', .) %>%
        gsub('^0.', '.', .) %>% gsub('^.000$', '<.001', .)
      
      # If interaction_symbol_x = TRUE, replace double colons with times 
      # symbols followed by line breaks and indentation. Then, replace 
      # single colons with times symbols.
      if(interaction_symbol_x) {
        model_summary$Effect = model_summary$Effect %>% 
          gsub('::', ' $\\\\times$ \n \\\\hspace{0.3cm}', .) %>%
          gsub(':', ' $\\\\times$ ', .)
      }
      
      model_summary$Effect = model_summary$Effect %>%
        
        # Escape underscores to avoid error in table
        str_replace_all('_', '\\\\_') %>%
        
        # Allow line breaks in the names of the effects
        # (used in the interactions)
        kableExtra::linebreak(align = 'l')
      
      # Select final variables to be shown, depending on whether 
      # confidence intervals available, and format names.
      if(is.null(confidence_intervals)) {
        model_summary = model_summary[, c('Effect', 'Estimate', 
                                          'SE', 't.value', 'p')] %>%
          rename('$\\upbeta$' = 'Estimate', '$SE$' = 'SE', 
                 '$t$' = 't.value', '$p$' = 'p')
      } else {
        model_summary = model_summary[, c('Effect', 'Estimate', 'SE', 
                                          'CI_95', 't.value', 'p')] %>%
          rename('$\\upbeta$' = 'Estimate', '$SE$' = 'SE', '95\\% CI' = 'CI_95', 
                 '$t$' = 't.value', '$p$' = 'p')
      }
      
      # Output table
      model_summary %>% 
        
        # Remove header of first column
        rename(' ' = 'Effect') %>%
        
        # Present table
        kbl(digits = 2, booktabs = TRUE, escape = FALSE, align = align,
            
            # Caption of the table (default unless specified)
            caption = caption, 
            
            # Disable occasional line gap (https://stackoverflow.com/a/49018919/7050882)
            linesep = '') %>%
        
        # Apply nice kableExtra format
        kable_styling() %>%
        
        # Center-align header row
        row_spec(0, align = 'c')
    }
  }



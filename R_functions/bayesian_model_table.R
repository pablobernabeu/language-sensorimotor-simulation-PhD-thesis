

# Function used in the manuscript to present summaries from 'brms' models 
# in APA-formatted tables. The only obligatory argument to be supplied is 
# a summary of a 'brms' model.

bayesian_model_table = 
  
  function(model_summary, show_intercept = TRUE, select_effects = NULL, 
           order_effects = NULL, format = NULL, 
           
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
      data.frame(Effect = rownames(model_summary$fixed), 
                 Estimate = model_summary$fixed$Estimate, 
                 SE = model_summary$fixed$Est.Error, 
                 CrI_2.5 = model_summary$fixed$`l-95% CI`, 
                 CrI_97.5 = model_summary$fixed$`u-95% CI`, 
                 Rhat = model_summary$fixed$Rhat,
                 row.names = NULL)
    
    # Process credible intervals and present both inside square brackets
    
    model_summary$CrI_2.5 = model_summary$CrI_2.5 %>% 
      # Round off and keep trailing zeros
      sprintf('%.2f', .) %>% 
      # Remove minus sign from pure zeros
      sub('-0.00', '0.00', .)
    
    model_summary$CrI_97.5 = model_summary$CrI_97.5 %>% 
      # Round off and keep trailing zeros
      sprintf('%.2f', .) %>% 
      # Remove minus sign from pure zeros
      sub('-0.00', '0.00', .)
    
    model_summary$CrI_95 = paste0('[', model_summary$CrI_2.5, ', ', 
                                 model_summary$CrI_97.5, ']')
    
    # If show_intercept = FALSE, remove it
    if(isFALSE(show_intercept)) {
      model_summary = model_summary %>% filter(!grepl('Intercept', Effect))
      
      # Put 'Intercept' in parentheses
    } else if(!is.null(model_summary[model_summary$Effect == 'Intercept', 'Effect'])) {
      model_summary[model_summary$Effect == 'Intercept', 'Effect'] = '(Intercept)'
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
    
    # Round other values
    
    model_summary$Estimate = model_summary$Estimate %>% as.numeric %>% 
      # Round off and keep trailing zeros
      sprintf('%.2f', .) %>% 
      # Remove minus sign from pure zeros
      sub('-0.00', '0.00', .)
    
    model_summary$SE = model_summary$SE %>% as.numeric %>% 
      # Round off and keep trailing zeros
      sprintf('%.2f', .)
    
    model_summary$Rhat = model_summary$Rhat %>% as.numeric %>% 
      # Round off and keep trailing zeros
      sprintf('%.2f', .)
    
    # Order columns
    model_summary = model_summary %>% select(Effect, Estimate, SE, CrI_95, Rhat)
    
    # Right-align all columns after first one
    align = c('l', 'r', 'r', 'r', 'r')
    
    # Establish latex or HTML format: if no format supplied, 
    # try to obtain it from knitr, or apply HTML
    if(missing(format) || is.null(format)) {
      if(knitr::is_latex_output()) {
        format = 'latex'
      } else format = 'html'
    }
    
    # HTML format
    if(format == 'html') {
      
      # If interaction_symbol_x = TRUE, replace double colons with times 
      # symbols followed by line breaks and indentation. Then, replace 
      # single colons with times symbols.
      if(interaction_symbol_x) {
        model_summary$Effect = model_summary$Effect %>% 
          gsub('::', ' &times; <br> &nbsp;&nbsp;', .) %>%
          gsub(':', ' &times; ', .)
      }
      
      # Output table
      model_summary %>% 
        
        # Remove header of first column and rename other headers
        rename(' ' = 'Effect', '&beta;' = 'Estimate', '<i>SE</i>' = 'SE', 
               '95% CrI' = 'CrI_95', '&Rcirc;' = 'Rhat') %>%
        
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
      
      # Output table
      model_summary %>% 
        
        # Remove header of first column and rename other headers
        rename(' ' = 'Effect', '$\\upbeta$' = 'Estimate', '$SE$' = 'SE', 
               '95\\% CrI' = 'CrI_95', '$\\widehat R$' = 'Rhat') %>%
        
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



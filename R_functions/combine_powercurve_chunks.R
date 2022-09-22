

# Function to unify the chunks of each power curve. This is useful for consolidating
# all the parts of a 'simr' power curve that were run in parallel to save time. This 
# function requires that chunks be named following the pattern below:
# 'powercurve<any_label>_<number>participants_<anything else>'. For instance: 
# 'powercurve1_50participants_200sim_semanticpriming_lmerTest.rds'

combine_powercurve_chunks = 
  
  function(files_list, powercurve_number) {
    
    # Select power curves matching the number supplied to this function
    files_list = files_list[str_detect(names(files_list), 
                                       paste0('powercurve', 
                                              powercurve_number, 
                                              '_.*'))]
    
    # Use the first chunk to create a structure that will be used to
    # process every chunk.
    powercurve = files_list[[1]]
    
    # Concatenate all the sample sizes examined ('nlevels')
    powercurve$nlevels = 
      names(files_list) %>% str_match('_\\s*(.*?)\\s*participants') %>% 
      data.frame %>% select(2) %>% unlist %>% as.numeric %>% unique %>% sort
    
    # Set appropriate values in the Y-axis, namely, the sample sizes 
    # instead of the number of rows (default in 'simr' package).
    powercurve$xval = powercurve$nlevels
    
    # Combine results
    for(i in 1 : n_distinct(powercurve$nlevels)) {
      powercurve$ps[i] = 
        files_list[str_detect(names(files_list), 
                              paste0('powercurve', powercurve_number, '_', 
                                     powercurve$nlevels[i], 
                                     'participants_'))][[1]]$ps
    }
    
    # Add up the running time of all chunks.
    # First, extract timing of first chunk
    timing = files_list[str_detect(names(files_list), 
                                   paste0('powercurve', powercurve_number, '_', 
                                          powercurve$nlevels[i], 
                                          'participants_'))][[1]]$timing
    
    # On top of the above timing, add up the timing of all subsequent chunks.
    # For this purpose, the loop starts from the position 2.
    for(i in 2 : n_distinct(powercurve$nlevels)) {
      timing = timing +
        files_list[str_detect(names(files_list), 
                              paste0('powercurve', powercurve_number, '_', 
                                     powercurve$nlevels[i], 
                                     'participants_'))][[1]]$timing
    }
    
    # Pass 'timing' into the timing slot of 'powercurve'
    powercurve$timing = timing
    
    # Output
    powercurve
    
  }


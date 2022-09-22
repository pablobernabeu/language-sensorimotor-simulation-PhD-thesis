

# Plotting residuals (for diagnosing models)

residuals_plot = function(model) {
  
  require(dplyr)          # data wrangling
  require(broom.mixed)    # accessing model output
  require(qqplotr)        # plotting
  require(Cairo)          # plot format

  broom.mixed::augment(model) %>%    # Format model output
    ggplot(., mapping = aes(sample = .resid)) +
    geom_qq_band(bandType = 'ks', mapping = aes(fill = 'KS'), alpha = 0.5) +
    geom_qq_band(bandType = 'ts', mapping = aes(fill = 'TS'), alpha = 0.5) +
    geom_qq_band(bandType = 'pointwise', mapping = aes(fill = 'Normal'), alpha = 0.5) +
    geom_qq_band(bandType = 'boot', mapping = aes(fill = 'Bootstrap'), alpha = 0.5) +
    stat_qq_line() + stat_qq_point() +
    labs(x = 'Theoretical quantiles', y = 'Sample quantiles') +
    scale_fill_discrete('Bandtype') +
    scale_x_continuous(expand = expansion(mult = .01)) + 
    theme_classic() + 
    theme(axis.title.x = element_text(size = 16, margin = margin(t = 6)),
          axis.title.y = element_text(size = 16, margin = margin(r = 6)), 
          axis.text = element_text(size = 14),
          legend.title = element_text(size = 16, hjust = .5, 
                                      margin = margin(b = unit(2, 'cm'))), 
          legend.text = element_text(size = 14, vjust = .5), 
          legend.position = c(.85, .2), legend.spacing.y = unit(6, 'pt'),
          plot.margin = margin(15, 15, 15, 15)) +
    
    # Allow use of `legend.spacing.y` above
    guides(fill = guide_legend(byrow = TRUE))
}


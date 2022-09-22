

# This function takes the output from brms::pp_check() and produces a plot.
# The appearance of this plot differs slightly from that of the default 
# 'pp_check' plot. For instance, the Y-axis labels are shown. 


plot_posteriorpredictivecheck = 
  
  function(pp_check) {
    
    require(dplyr)
    require(ggplot2)
    require(ggtext)
    
    pp_check + 
      xlab('Estimate') + ylab('Density') +
      # Show around 6 tick labels in each axis and set axis padding 
      # (padding curtailed by the settings of the original plot)
      scale_x_continuous(n.breaks = 6, expand = c(0, 0)) + 
      scale_y_continuous(n.breaks = 6, expand = c(2, 0.8)) +
      theme_minimal() +
      theme(text = element_text(family = 'Arial'),  # <-- prevent R crashing (see https://discourse.mc-stan.org/t/error-code-when-using-pp-check-with-brms/27419/4) 
            axis.title.x = element_markdown(size = 15, margin = margin(t = 6)),
            axis.text.x = element_text(size = 13, margin = margin(t = 4)),
            axis.title.y = element_text(size = 15, margin = margin(r = 9)),
            axis.text.y = element_text(size = 13, margin = margin(r = 4)),
            axis.ticks = element_line(), 
            legend.text = element_text(size = 20, hjust = 0), 
            legend.position = c(.85, .7), legend.key.size = unit(1.4, 'cm'), 
            legend.key.height = unit(1.2, 'cm'),
            legend.background = element_rect(color = 'grey80', fill = 'white'),
            panel.grid.minor = element_blank(),
            plot.margin = margin(12, 4, 14, 12),
            plot.title = element_markdown(size = 16, hjust = 0.5, vjust = 1, 
                                          margin = margin(b = 9)))
    
  }


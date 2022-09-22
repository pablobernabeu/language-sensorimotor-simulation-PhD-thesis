

# The target function, correlation_matrix(), will be defined after the 
# intermediary function, GGally::ggcorr().


# 1. GGally::ggcorr()

# Adapted from the 'ggcorr' function from the 'GGally' package (Version 2.1.2;
# Briatte, 2021, https://cran.r-project.org/web/packages/GGally/GGally.pdf)
# This adaptation was required to disable the default replacement of 
# white spaces with underscores. The code for this adaptation was shared by 
# Nami Sunami on Github (https://github.com/briatte/ggcorr/pull/15).

ggcorr <- function(
    data,
    method = c("pairwise", "pearson"),
    cor_matrix = NULL,
    nbreaks = NULL,
    digits = 2,
    name = "",
    low = "#3B9AB2",
    mid = "#EEEEEE",
    high = "#F21A00",
    midpoint = 0,
    palette = NULL,
    geom = "tile",
    min_size = 2,
    max_size = 6,
    label = FALSE,
    label_alpha = FALSE,
    label_color = "black",
    label_round = 1,
    label_size = 4,
    limits = TRUE,
    drop = !limits,
    layout.exp = 0,
    legend.position = "right",
    legend.size = 9,
    ...) {
  
  # -- required packages -------------------------------------------------------
  
  require(ggplot2, quietly = TRUE)
  require(reshape2, quietly = TRUE)
  require(tibble, quietly = TRUE)
  
  # -- check geom argument -----------------------------------------------------
  
  if (length(geom) > 1 || !geom %in% c("blank", "circle", "text", "tile")) {
    stop("incorrect geom value")
  }
  
  # -- correlation method ------------------------------------------------------
  
  if (length(method) == 1) {
    method = c(method, "pearson") # for backwards compatibility
  }
  
  # -- check data columns ------------------------------------------------------
  
  if (!is.null(data)) {
    
    if (!is.data.frame(data)) {
      data = as.data.frame(data)
    }
    
    x = which(!sapply(data, is.numeric))
    
    if (length(x) > 0) {
      
      warning(paste("data in column(s)",
                    paste0(paste0("'", names(data)[x], "'"), collapse = ", "),
                    "are not numeric and were ignored"))
      
      data = data[, -x ]
      
    }
    
  }
  
  # -- correlation matrix ------------------------------------------------------
  
  if (is.null(cor_matrix)) {
    cor_matrix = cor(data, use = method[1], method = method[2])
  }
  
  m = cor_matrix
  
  # -- correlation data.frame --------------------------------------------------
  
  m = as_tibble(m * lower.tri(m))
  m$.ggally_ggcorr_row_names = colnames(m)
  m = reshape2::melt(m, id.vars = ".ggally_ggcorr_row_names")
  names(m) = c("x", "y", "coefficient")
  m$coefficient[ m$coefficient == 0 ] = NA
  
  # -- correlation quantiles ---------------------------------------------------
  
  if (!is.null(nbreaks)) {
    
    x = seq(-1, 1, length.out = nbreaks + 1)
    
    if (!nbreaks %% 2) {
      x = sort(c(x, 0))
    }
    
    m$breaks = cut(m$coefficient, breaks = unique(x), include.lowest = TRUE,
                   dig.lab = digits)
    
  }
  
  # -- gradient midpoint -------------------------------------------------------
  
  if (is.null(midpoint)) {
    
    midpoint = median(m$coefficient, na.rm = TRUE)
    message(paste("Color gradient midpoint set at median correlation to",
                  round(midpoint, 2)))
    
  }
  
  # -- plot structure ----------------------------------------------------------
  
  m$label = round(m$coefficient, label_round)
  p = ggplot(na.omit(m), aes(x, y))
  
  if (geom == "tile") {
    
    if (is.null(nbreaks)) {
      
      # -- tiles, continuous ---------------------------------------------------
      
      p = p +
        geom_tile(aes(fill = coefficient), color = "white")
      
    } else {
      
      # -- tiles, ordinal ------------------------------------------------------
      
      p = p +
        geom_tile(aes(fill = breaks), color = "white")
      
    }
    
    # -- tiles, color scale ----------------------------------------------------
    
    if (is.null(nbreaks) && limits) {
      
      p = p +
        scale_fill_gradient2(name, low = low, mid = mid, high = high,
                             midpoint = midpoint, limits = c(-1, 1))
      
    } else if (is.null(nbreaks)) {
      
      p = p +
        scale_fill_gradient2(name, low = low, mid = mid, high = high,
                             midpoint = midpoint)
      
    } else if (is.null(palette)) {
      
      x = colorRampPalette(c(low, mid, high))(length(levels(m$breaks)))
      
      p = p +
        scale_fill_manual(name, values = x, drop = drop)
      
    } else {
      
      p = p +
        scale_fill_brewer(name, palette = palette, drop = drop)
      
    }
    
  } else if (geom == "circle") {
    
    p = p +
      geom_point(aes(size = abs(coefficient) * 1.25), color = "grey50") # border
    
    if (is.null(nbreaks)) {
      
      # -- circles, continuous -------------------------------------------------
      
      p = p +
        geom_point(aes(size = abs(coefficient), color = coefficient))
      
    } else {
      
      # -- circles, ordinal ----------------------------------------------------
      
      p = p +
        geom_point(aes(size = abs(coefficient), color = breaks))
      
    }
    
    p = p +
      scale_size_continuous(range = c(min_size, max_size)) +
      guides(size = FALSE)
    
    r = list(size = (min_size + max_size) / 2)
    
    # -- circles, color scale --------------------------------------------------
    
    if (is.null(nbreaks) && limits) {
      
      p = p +
        scale_color_gradient2(name, low = low, mid = mid, high = high,
                              midpoint = midpoint, limits = c(-1, 1))
      
    } else if (is.null(nbreaks)) {
      
      p = p +
        scale_color_gradient2(name, low = low, mid = mid, high = high,
                              midpoint = midpoint)
      
    } else if (is.null(palette)) {
      
      x = colorRampPalette(c(low, mid, high))(length(levels(m$breaks)))
      
      p = p +
        scale_color_manual(name, values = x, drop = drop) +
        guides(color = guide_legend(override.aes = r))
      
    } else {
      
      p = p +
        scale_color_brewer(name, palette = palette, drop = drop) +
        guides(color = guide_legend(override.aes = r))
      
    }
    
  } else if (geom == "text") {
    
    if (is.null(nbreaks)) {
      
      # -- text, continuous ----------------------------------------------------
      
      p = p +
        geom_text(aes(label = label, color = coefficient), size = label_size)
      
    } else {
      
      # -- text, ordinal -------------------------------------------------------
      
      p = p +
        geom_text(aes(label = label, color = breaks), size = label_size)
      
    }
    
    # -- text, color scale ----------------------------------------------------
    
    if (is.null(nbreaks) && limits) {
      
      p = p +
        scale_color_gradient2(name, low = low, mid = mid, high = high,
                              midpoint = midpoint, limits = c(-1, 1))
      
    } else if (is.null(nbreaks)) {
      
      p = p +
        scale_color_gradient2(name, low = low, mid = mid, high = high,
                              midpoint = midpoint)
      
    } else if (is.null(palette)) {
      
      x = colorRampPalette(c(low, mid, high))(length(levels(m$breaks)))
      
      p = p +
        scale_color_manual(name, values = x, drop = drop)
      
    } else {
      
      p = p +
        scale_color_brewer(name, palette = palette, drop = drop)
      
    }
    
  }
  
  # -- coefficient labels ------------------------------------------------------
  
  if (label) {
    
    if (isTRUE(label_alpha)) {
      
      p = p +
        geom_text(aes(x, y, label = label, alpha = abs(coefficient)),
                  color = label_color, size = label_size,
                  show_guide = FALSE)
      
    } else if (label_alpha > 0) {
      
      p = p +
        geom_text(aes(x, y, label = label, show_guide = FALSE),
                  alpha = label_alpha, color = label_color, size = label_size)
      
    } else {
      
      p = p +
        geom_text(aes(x, y, label = label),
                  color = label_color, size = label_size)
      
    }
    
  }
  
  # -- horizontal scale expansion ----------------------------------------------
  
  l = levels(m$y)
  
  if (!is.numeric(layout.exp) || layout.exp < 0) {
    stop("incorrect layout.exp value")
  } else if (layout.exp > 0) {
    l = c(rep(NA, as.integer(layout.exp)), l)
  }
  
  p = p  +
    geom_text(data = m[ m$x == m$y & is.na(m$coefficient), ],
              aes(label = x), ...) +
    scale_x_discrete(breaks = NULL, limits = l) +
    scale_y_discrete(breaks = NULL, limits = levels(m$y)) +
    labs(x = NULL, y = NULL) +
    coord_equal() +
    theme(
      panel.background = element_blank(),
      legend.key = element_blank(),
      legend.position = legend.position,
      legend.title = element_text(size = legend.size),
      legend.text = element_text(size = legend.size)
    )
  
  return(p)
}

################################################################################


# 2. correlation_matrix()

# Custom correlation matrix based on GGally::ggcorr(). Once the function is run, 
# it is important to adjust the margins of the plot using theme(). Example:
# 
# semanticdecision[, c('z_RTclean', 'z_vocabulary_size', 'z_information_uptake', 
#                      'z_word_cooccurrence', 'z_visual_rating', 
#                      'z_word_concreteness', 'z_word_frequency', 
#                      'z_orthographic_Levenshtein_distance')] %>%
#   correlation_matrix() + 
#   theme(plot.margin = unit(c(0, 0, 0.05, -2.9), 'in'))


correlation_matrix = function(data) {
  
  require(dplyr)
  require(GGally)
  require(ggplot2)
  
  ggcorr(data, digits = 10, label_round = 10, hjust = .93, 
         layout.exp = 7, size = 6, geom = 'blank') +
    
    geom_label(
      aes(
        # Format coefficient label by rounding off, then removing minus signs 
        # from 0.00 coefficients, and removing leading zeros.
        label = label %>% 
          sprintf('%.2f', .) %>% sub('-0.00', '0.00', .) %>% 
          sub('^(-)?0[.]', '\\1.', .),
        
        # Below, the object `coefficient`--internally created by ggcorr--contains the 
        # correlations. Prepare settings for size and colour of the coefficients
        size = abs(coefficient), alpha = abs(coefficient),
        
        # Create negative/positive categories to ensure consistent colour across plots.
        fill = ifelse(coefficient < 0, 'negative', 
                      ifelse(coefficient == 0, 'zero', 'positive'))
      ), 
      
      color = alpha('black', 1), label.padding = unit(0.1, 'in'), label.size = NA, 
      label.r = unit(0.2, 'in'), nudge_y = -0.03
    ) +
    
    # Size of each rectangle
    scale_size('coefficient', range = c(4, 5.3), limits = 0:1, breaks = 0:1) +
    
    # Colour of each rectangle
    scale_fill_manual(values = c(negative = 'blue', zero = 'white', positive = 'red')) +
    
    # Intensity of the colour
    scale_alpha('coefficient', range = c(0, .5), limits = 0:1, breaks = 0:1) +
    
    theme(legend.position = 'none')
}



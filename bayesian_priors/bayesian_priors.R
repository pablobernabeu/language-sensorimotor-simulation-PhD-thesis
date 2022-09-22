

# Priors used in the Bayesian analyses


library(dplyr)
library(ggplot2)
library(ggridges)
library(ggtext)

# Set seed number to ensure exact reproducibility 
# of the random distributions
set.seed(123)


# Background

# The priors below were established by inspecting the effect sizes obtained
# in previous studies as well as the effect sizes obtained in our 
# frequentist analyses of the present studies, which are contained in the 
# present project. These sources revealed highly-similar ranges. The 
# previous studies that were considered were selected because they 
# implemented experimental experimental paradigms and analytical procedures 
# similar to those used in the current studies. The paradigms were lexical 
# decision, semantic decision or semantic priming with a lexical decision 
# task. The analytical procedures consisted of the z-scoring of the 
# dependent and the independent variables. Two studies matched these 
# characteristics:
# -- Pexman and Yap (2018, Tables 6 and 7, https://doi.org/10.1037/xlm0000499s)
# -- Lim et al. (2020, Table 5, https://doi.org/10.1177/1747021820906566)
# These studies and the frequentist analyses contained in the present 
# project yielded effect sizes smaller than ±0.30. The bounds of this 
# range were determined by determined by the results from Pexman and Yap 
# (2018), who found a word-concreteness effect of β = 0.41 in the 
# concrete-words analysis, and an effect of β = 0.20 in the abstract-words 
# analysis. Since we did not separate abstract from concrete words, we 
# averaged the former values, and set -0.30 as the lower bound, and 0.30 
# as the upper bound.

# Direction of effects

# The sources used to determine the effect size range also revealed that 
# some effects consistently presented a negative direction (e.g., word 
# frequency), whereas some other effects were consistently positive 
# (e.g., number of letters and interstimulus interval). We incorporated 
# direction information into the priors in cases of very consistent 
# patterns, whereas most priors were non-directional. For instance, we 
# did not incorporate information about the direction of the word 
# concreteness effect, as it can vary within various sections of the 
# data (Pexman & Yap, 2018). Some previous studies have integrated 
# effect direction in some priors (Stone et al., 2021, 
# https://doi.org/10.1080/23273798.2021.1921816), but most have not
# (Pregla et al., 2021, https://doi.org/10.1016/j.bandl.2021.105008; 
# Rodríguez-Ferreiro et al., 2020, https://doi.org/10.7717/peerj.9511; 
# Stone et al., 2020, https://doi.org/10.7717/peerj.10438).

# Prior predictive checks

# The range of priors established on this basis consisted of an 
# informative prior (SD = 0.1), a weakly-informative prior 
# (SD = 0.2) and a diffuse prior (SD = 0.3). The adequacy of each 
# of these priors was tested by means of prior predictive checks 
# (see 'bayesian_analysis' folder in each study). The results of 
# these checks suggested that these priors were adequate and that 
# an ex-gaussian distribution should be applied in the models, 
# converging with the findings of Rodríguez-Ferreiro et al. (2020; 
# see supplementary materials via https://doi.org/10.7717/peerj.9511).

# The present range of priors broadly resembles the priors used in 
# previous psycholinguistic studies (Pregla et al., 2021, 
# https://doi.org/10.1016/j.bandl.2021.105008; Stone et al., 2020, 
# https://doi.org/10.7717/peerj.10438; Stone et al., 2021, 
# https://doi.org/10.1080/23273798.2021.1921816). For instance, 
# Stone et al.'s (2020) priors were: Normal(0, 0.1), Normal(0, 0.3) 
# and Normal(0, 1). 

# Prior-sensitivity analyses

# The results based on each of these priors were compared, in the main 
# analyses, to acknowledge the well-known influence of priors on 
# results (Lee & Wagenmakers, 2014, https://doi.org/10.1017/CBO9781139087759; 
# Stone et al., 2020, https://doi.org/10.7717/peerj.10438). These 
# priors were formed of an informative one, a weakly-informative one 
# and a diffuse one. These three types of priors were run in separate 
# models. Thus, in each model, all priors had the same degree of 
# informativeness (for similar approaches, see Pregla et al., 2021, 
# https://doi.org/10.1016/j.bandl.2021.105008; Rodríguez-Ferreiro et 
# al., 2020, https://doi.org/10.7717/peerj.9511; Stone et al., 2020, 
# https://doi.org/10.7717/peerj.10438; Stone et al., 2021, 
# https://doi.org/10.1080/23273798.2021.1921816).

####################################################################


# This code

# The code below plots all our types of priors. Each distribution 
# contains 10,000 simulations, resulting in 90,000 rows.

# The green vertical rectangle shows the range of plausible effect 
# sizes based on previous studies that applied a similar analysis 
# (Lim et al., 2020, https://doi.org/10.1177/1747021820906566; 
# Pexman & Yap, 2018, https://doi.org/10.1037/xlm0000499) as 
# well as on the frequentist analyses of the current data.


priors = data.frame(
  
  informativeness = 
    as.factor(c(rep('Informative priors (*SD* = 0.1)', 30000),
                rep('Weakly-informative priors (*SD* = 0.2)', 30000),
                rep('Diffuse priors (*SD* = 0.3)', 30000))), 
  
  direction = as.factor(c(rep('negative', 10000), 
                          rep('neutral', 10000),
                          rep('positive', 10000),
                          rep('negative', 10000), 
                          rep('neutral', 10000),
                          rep('positive', 10000),
                          rep('negative', 10000), 
                          rep('neutral', 10000),
                          rep('positive', 10000))),
  
  direction_and_distribution = 
    as.factor(c(rep('Negative (*M* = -0.1)<br>*Normal*(-0.1, 0.1)', 10000), 
                rep('Neutral (*M* = 0)<br>*Normal*(0, 0.1)', 10000),
                rep('Positive (*M* = 0.1)<br>*Normal*(0.1, 0.1)', 10000),
                rep('Negative (*M* = -0.1)<br>*Normal*(-0.1, 0.2)', 10000),
                rep('Neutral (*M* = 0)<br>*Normal*(0, 0.2)', 10000),
                rep('Positive (*M* = 0.1)<br>*Normal*(0.1, 0.2)', 10000),
                rep('Negative (*M* = -0.1)<br>*Normal*(-0.1, 0.3)', 10000), 
                rep('Neutral (*M* = 0)<br>*Normal*(0, 0.3)', 10000),
                rep('Positive (*M* = 0.1)<br>*Normal*(0.1, 0.3)', 10000))),
  
  estimate = c(rnorm(10000, m = -0.1, sd = 0.1),
               rnorm(10000, m = 0, sd = 0.1),
               rnorm(10000, m = 0.1, sd = 0.1),
               rnorm(10000, m = -0.1, sd = 0.2),
               rnorm(10000, m = 0, sd = 0.2),
               rnorm(10000, m = 0.1, sd = 0.2),
               rnorm(10000, m = -0.1, sd = 0.3),
               rnorm(10000, m = 0, sd = 0.3),
               rnorm(10000, m = 0.1, sd = 0.3))
)

# Order factor levels

priors$informativeness = 
  ordered(priors$informativeness, 
          levels = c('Informative priors (*SD* = 0.1)', 
                     'Weakly-informative priors (*SD* = 0.2)', 
                     'Diffuse priors (*SD* = 0.3)'))

priors$direction = 
  ordered(priors$direction, 
          levels = c('negative', 'neutral', 'positive'))

priors$direction_and_distribution =
  ordered(priors$direction_and_distribution,
          levels = c('Negative (*M* = -0.1)<br>*Normal*(-0.1, 0.1)', 
                     'Neutral (*M* = 0)<br>*Normal*(0, 0.1)',
                     'Positive (*M* = 0.1)<br>*Normal*(0.1, 0.1)',
                     'Negative (*M* = -0.1)<br>*Normal*(-0.1, 0.2)', 
                     'Neutral (*M* = 0)<br>*Normal*(0, 0.2)',
                     'Positive (*M* = 0.1)<br>*Normal*(0.1, 0.2)',
                     'Negative (*M* = -0.1)<br>*Normal*(-0.1, 0.3)', 
                     'Neutral (*M* = 0)<br>*Normal*(0, 0.3)',
                     'Positive (*M* = 0.1)<br>*Normal*(0.1, 0.3)'))


# PLOT zone

colours = c('#7276A2', 'black', '#A27272')
fill_colours = c('#CCCBE7', '#D7D7D7', '#E7CBCB')

# Initialise plot (`aes` specified separately to allow 
# use of `geom_rect` at the end)
(
  ggplot() +
    
    # Turn to the distributions
    stat_density_ridges(data = priors, 
                        aes(x = estimate, y = direction_and_distribution, 
                            color = direction, fill = direction),
                        geom = 'density_ridges_gradient', alpha = 0.7, 
                        jittered_points = TRUE, quantile_lines = TRUE, 
                        quantiles = c(0.025, 0.975), show.legend = F) +
    scale_color_manual(values = colours) + 
    scale_fill_manual(values = fill_colours) + 
    # Adjust X axis to the random distributions obtained
    scale_x_continuous(limits = c(min(priors$estimate), 
                                  max(priors$estimate)), 
                       n.breaks = 6, expand = c(0.04, 0.04)) +
    scale_y_discrete(expand = expansion(add = c(0.18, 1.9))) +
    # Facets containing the three models varying in informativeness
    facet_wrap(vars(informativeness), scales = 'free', dir = 'v') +
    # Vertical line at x = 0
    geom_vline(xintercept = 0, linetype = 'dashed', color = 'grey50') +
    xlab('Effect size (&beta;)') + 
    ylab('Direction of the prior and corresponding distribution') +
    theme_minimal() +
    theme(axis.title.x = ggtext::element_markdown(size = 12, margin = margin(t = 9)),
          axis.text.x = ggtext::element_markdown(size = 11, margin = margin(t = 4)),
          axis.title.y = ggtext::element_markdown(size = 12, margin = margin(r = 9)),
          axis.text.y = ggtext::element_markdown(lineheight = 1.6, colour = colours),
          strip.background = element_rect(fill = 'grey98', colour = 'grey90',
                                          linetype = 'solid'),
          strip.text = element_markdown(size = 11, margin = margin(t = 7, b = 7)),
          panel.spacing.y = unit(9, 'pt'), panel.grid.minor = element_blank(), 
          plot.margin = margin(8, 8, 9, 8)
    ) +
    
    # Shaded rectangle containing range of previous effects
    geom_rect(data = data.frame(x = 1), xmin = -0.3, xmax = 0.3, 
              ymin = -Inf, ymax = Inf, fill = 'darkgreen', alpha = .3)
) %>%
  
  # Save plot to disk 
  ggsave(filename = 'bayesian_priors/plots/bayesian_priors.pdf', 
         device = cairo_pdf, bg = 'white', width = 5, height = 8, dpi = 900)




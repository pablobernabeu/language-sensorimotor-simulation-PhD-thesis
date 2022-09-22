

# Part of Study 1: Semantic priming

# Part of Bayesian analysis

# Prior predictive checks are performed to verify the adequacy of the priors
# (Nicenboim et al., 2022, https://vasishth.github.io/bayescogsci). For this 
# purpose, the results from three priors are compared below. These priors 
# differ in their standard deviations, which are—respectively—0.1 (priors 
# labelled 'informative'), 0.2 ('weakly-informative') and 0.3 ('diffuse'). 
# The rationale for these priors is provided in the 'bayesian_priors' 
# folder, located in the root directory of this project. An ex-gaussian 
# distribution is tested in this script (Rodríguez-Ferreiro et al., 2020; 
# see supplementary materials via https://doi.org/10.7717/peerj.9511).
# Due to computational constraints, the random-effects structures of these
# models are slightly smaller than those of the final models, and the
# number of iterations is lower.


library(dplyr)
library(brms)  # Version 2.17.0
library(ggplot2)
library(ggtext)
library(patchwork)


# Begin by checking what parameters of the model can be assigned priors

# Read in data
semanticpriming = read.csv('semanticpriming/data/final_dataset/semanticpriming.csv')


get_prior(
  
  # Dependent variable
  z_target.RT ~
    
    # FIXED EFFECTS
    
    # Controlled lexical variables
    z_target_word_frequency + z_target_number_syllables +
    
    # Controlled semantic variable
    z_word_concreteness_diff +
    
    # Control interactions with word concreteness to allow a rigorous analysis of the
    # interactions with visual-strength difference that are specified further below.
    z_word_concreteness_diff : z_vocabulary_size +
    z_word_concreteness_diff : z_recoded_interstimulus_interval +
    z_word_concreteness_diff : z_recoded_participant_gender +
    
    # Important control due to the relationship between this variable and vocabulary size
    # (Yap et al., 2017; https://www.montana.edu/khutchison/documents/YHT%20in%20press.pdf)
    z_attentional_control +
    
    # Control interactions with attentional control to allow a rigorous analysis of
    # the interactions with vocabulary size specified further below.
    z_cosine_similarity : z_attentional_control +
    z_visual_rating_diff : z_attentional_control +
    
    # Main effects of interest
    z_vocabulary_size + z_recoded_participant_gender + z_cosine_similarity +
    z_visual_rating_diff + z_recoded_interstimulus_interval +
    
    # Interactions of interest
    z_cosine_similarity : z_vocabulary_size +
    z_visual_rating_diff : z_vocabulary_size +
    z_cosine_similarity : z_recoded_participant_gender +
    z_visual_rating_diff : z_recoded_participant_gender +
    z_cosine_similarity : z_recoded_interstimulus_interval +
    z_visual_rating_diff : z_recoded_interstimulus_interval +
    
    # RANDOM EFFECTS: maximal structure constructed following the guidelines of Brauer and Curtin (2018;
    # https://psych.wisc.edu/Brauer/BrauerLab/wp-content/uploads/2014/04/Brauer-Curtin-2018-on-LMEMs.pdf).
    # Interactions only require random slopes if all variables involved vary within the same units.
    
    # Random intercepts
    (1 | Participant) + (1 | primeword_targetword) +
    
    # In the random slopes below, `0 +` helps avoid redundant random intercepts
    # (background: https://github.com/lme4/lme4/issues/625)
    
    # By-participant random slopes
    (0 + z_cosine_similarity | Participant) +
    (0 + z_visual_rating_diff | Participant) +
    (0 + z_recoded_interstimulus_interval | Participant) +
    (0 + z_cosine_similarity : z_recoded_interstimulus_interval | Participant) +
    (0 + z_visual_rating_diff : z_recoded_interstimulus_interval | Participant) +
    (0 + z_target_word_frequency | Participant) +
    (0 + z_target_number_syllables | Participant) +
    (0 + z_word_concreteness_diff | Participant) +
    
    # Random slopes by prime-target pair
    (0 + z_vocabulary_size | primeword_targetword) +
    (0 + z_recoded_participant_gender | primeword_targetword) +
    (0 + z_attentional_control | primeword_targetword),
  
  data = semanticpriming,
  
  # Following the prior predictive checks, an ex-gaussian distribution is set below.
  # This distribution is well suited for the analysis of response times (Bürkner et al., 
  # 2022, https://cran.r-project.org/web/packages/brms/brms.pdf; Rodríguez-Ferreiro 
  # et al., 2020, see supplementary materials via https://doi.org/10.7717/peerj.9511). 
  # Furthermore, the use of link functions, instead of transformations of the 
  # dependent variable, largely preserves the interpretability of the coefficients 
  # (Knief & Forstmeier, 2021; Lo & Andrews, 2015, https://doi.org/10.3389/fpsyg.2015.01171).
  
  family = exgaussian()
)

# ############################################################################


# PRIOR PREDICTIVE CHECK WITH WEAKLY-INFORMATIVE PRIORS

# Define priors

# In the function `set_prior` used below, the argument `class` often represents
# groups of parameters. For instance, class 'b' refers to fixed effects.

# Since most priors do not specify the negative/positive direction of effects,
# distributions are first specified for entire classes, and those are
# overridden wherever a direction is specified.

weaklyinformative_priors =
  c(set_prior('normal(0, 0.2)', class = 'Intercept'),
    set_prior('normal(0, 0.2)', class = 'b'),
    set_prior('normal(0.1, 0.2)', class = 'b',
              coef = 'z_target_number_syllables'),
    set_prior('normal(-0.1, 0.2)', class = 'b',
              coef = 'z_target_word_frequency'),
    set_prior('normal(0, 0.2)', class = 'sd'),  # automatically truncated to keep positive values only
    set_prior('normal(0, 0.2)', class = 'sigma')  # automatically truncated to keep positive values only
  )

# Perform a prior predictive check following Nicenboim et al. (2022, Section
# https://vasishth.github.io/bayescogsci/book/sec-ppd.html#sec:lognormal).

# Model

sampleonly_semanticpriming_brms_weaklyinformativepriors =
  
  brm(
    
    # Dependent variable
    z_target.RT ~
      
      # FIXED EFFECTS
      
      # Controlled lexical variables
      z_target_word_frequency + z_target_number_syllables +
      
      # Controlled semantic variable
      z_word_concreteness_diff +
      
      # Control interactions with word concreteness to allow a rigorous analysis of the
      # interactions with visual-strength difference that are specified further below.
      z_word_concreteness_diff : z_vocabulary_size +
      z_word_concreteness_diff : z_recoded_interstimulus_interval +
      z_word_concreteness_diff : z_recoded_participant_gender +
      
      # Important control due to the relationship between this variable and vocabulary size
      # (Yap et al., 2017; https://www.montana.edu/khutchison/documents/YHT%20in%20press.pdf)
      z_attentional_control +
      
      # Control interactions with attentional control to allow a rigorous analysis of
      # the interactions with vocabulary size specified further below.
      z_cosine_similarity : z_attentional_control +
      z_visual_rating_diff : z_attentional_control +
      
      # Main effects of interest
      z_vocabulary_size + z_recoded_participant_gender + z_cosine_similarity +
      z_visual_rating_diff + z_recoded_interstimulus_interval +
      
      # Interactions of interest
      z_cosine_similarity : z_vocabulary_size +
      z_visual_rating_diff : z_vocabulary_size +
      z_cosine_similarity : z_recoded_participant_gender +
      z_visual_rating_diff : z_recoded_participant_gender +
      z_cosine_similarity : z_recoded_interstimulus_interval +
      z_visual_rating_diff : z_recoded_interstimulus_interval +
      
      # RANDOM EFFECTS: maximal structure constructed following the guidelines of Brauer and Curtin (2018;
      # https://psych.wisc.edu/Brauer/BrauerLab/wp-content/uploads/2014/04/Brauer-Curtin-2018-on-LMEMs.pdf).
      # Interactions only require random slopes if all variables involved vary within the same units.
      
      # Random intercepts
      (1 | Participant) + (1 | primeword_targetword) +
      
      # In the random slopes below, `0 +` helps avoid redundant random intercepts
      # (background: https://github.com/lme4/lme4/issues/625)
      
      # By-participant random slopes
      (0 + z_cosine_similarity | Participant) +
      (0 + z_visual_rating_diff | Participant) +
      (0 + z_recoded_interstimulus_interval | Participant) +
      (0 + z_cosine_similarity : z_recoded_interstimulus_interval | Participant) +
      (0 + z_visual_rating_diff : z_recoded_interstimulus_interval | Participant) +
      (0 + z_target_word_frequency | Participant) +
      (0 + z_target_number_syllables | Participant) +
      (0 + z_word_concreteness_diff | Participant) +
      
      # Random slopes by prime-target pair
      (0 + z_vocabulary_size | primeword_targetword) +
      (0 + z_recoded_participant_gender | primeword_targetword) +
      (0 + z_attentional_control | primeword_targetword),
    
    data = semanticpriming, prior = weaklyinformative_priors,
    
    # Following the prior predictive checks, an ex-gaussian distribution is set below.
    # This distribution is well suited for the analysis of response times (Bürkner et al., 
    # 2022, https://cran.r-project.org/web/packages/brms/brms.pdf; Rodríguez-Ferreiro 
    # et al., 2020, see supplementary materials via https://doi.org/10.7717/peerj.9511). 
    # Furthermore, the use of link functions, instead of transformations of the 
    # dependent variable, largely preserves the interpretability of the coefficients 
    # (Knief & Forstmeier, 2021; Lo & Andrews, 2015, https://doi.org/10.3389/fpsyg.2015.01171).
    
    family = exgaussian(),
    
    # This model is used below to perform a prior predictive check
    sample_prior = 'only',
    
    seed = 123,  # allow exact replication of results
    warmup = 1000,  # standard number of warmup iterations
    iter = 2000, chains = 4,  # product of these numbers = total number of iterations
    cores = 4,  # parallel computation
    control = list(adapt_delta = 0.99, max_treedepth = 15)  # facilitate convergence
  )

# Plot prior predictive check. Code adapted from Nicenboim et al. (2022;
# https://vasishth.github.io/bayescogsci/book/sec-ppd.html#sec:lognormal)

p1 = pp_check(sampleonly_semanticpriming_brms_weaklyinformativepriors,
              type = 'stat', stat = 'min') +
  xlab('Response time (*z*)') +
  ggtitle('Prior predictive distribution of minimum values') +
  theme(axis.title.x = ggtext::element_markdown())

p2 = pp_check(sampleonly_semanticpriming_brms_weaklyinformativepriors,
              type = 'stat', stat = 'mean') +
  xlab('Response time (*z*)') +
  ggtitle('Prior predictive distribution of means') +
  theme(axis.title.x = ggtext::element_markdown())

p3 = pp_check(sampleonly_semanticpriming_brms_weaklyinformativepriors,
              type = 'stat', stat = 'max') +
  xlab('Response time (*z*)') +
  ggtitle('Prior predictive distribution of maximum values') +
  theme(axis.title.x = ggtext::element_markdown())

# Free up workspace
# rm(sampleonly_semanticpriming_brms_weaklyinformativepriors)

p123 = p1 + p2 + p3 + plot_layout(ncol = 1)

ggsave(p123,
       filename = 'semanticpriming/bayesian_analysis/prior_predictive_checks/plots/semanticpriming_priorpredictivecheck_weaklyinformativepriors_exgaussian.pdf',
       device = cairo_pdf, width = 7, height = 7, dpi = 900)

# Free up workspace
# rm(p1); rm(p2); rm(p3); rm(p123)





# Part of Study 2: Semantic decision

# Part of Bayesian analysis

# Prior predictive checks are performed to verify the adequacy of the priors
# (Nicenboim et al., 2022, https://vasishth.github.io/bayescogsci). For this 
# purpose, the results from three priors are compared below. These priors 
# differ in their standard deviations, which are—respectively—0.1 (priors 
# labelled 'informative'), 0.2 ('weakly-informative') and 0.3 ('diffuse'). 
# The rationale for these priors is provided in the 'bayesian_priors' 
# folder, located in the root directory of this project. A gaussian 
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
semanticdecision = read.csv('semanticdecision/data/final_dataset/semanticdecision.csv')


get_prior(
  
  # Dependent variable
  z_RTclean ~
    
    # FIXED EFFECTS
    
    # Controlled lexical variables
    z_word_frequency + z_orthographic_Levenshtein_distance +
    
    # Controlled word concreteness, important because the task is a word concreteness assessment.
    z_word_concreteness +
    
    # Control interaction between word concreteness and vocabulary size to allow a rigorous analysis of the
    # interaction between word co-occurrence and vocabulary size, and the interaction between word visual
    # rating and vocabulary size.
    z_word_concreteness : z_vocabulary_size +
    
    # Control interaction between word concreteness and participant gender to allow a rigorous analysis
    # of the interaction between visual rating and participant gender.
    z_word_concreteness : z_recoded_participant_gender +
    
    # Important control due to the relationship between this variable and vocabulary size (Pexman & Yap, 2018;
    # http://germel.dyndns.org/psyling/pdf/2018_PexmanYap_SemCat_IndividualDifferences.pdf)
    z_information_uptake +
    
    # Control interactions with information uptake to allow a rigorous analysis of the interactions with
    # vocabulary size specified below.
    z_word_cooccurrence : z_information_uptake +
    z_visual_rating : z_information_uptake +
    
    # Main effects of interest
    z_vocabulary_size + z_recoded_participant_gender +
    z_word_cooccurrence + z_visual_rating +
    
    # Interactions of interest
    z_word_cooccurrence : z_vocabulary_size +
    z_visual_rating : z_vocabulary_size +
    z_word_cooccurrence : z_recoded_participant_gender +
    z_visual_rating : z_recoded_participant_gender +
    
    # RANDOM EFFECTS: maximal structure constructed following the guidelines of Brauer and Curtin (2018;
    # https://psych.wisc.edu/Brauer/BrauerLab/wp-content/uploads/2014/04/Brauer-Curtin-2018-on-LMEMs.pdf).
    # Interactions only require random slopes if all variables involved vary within the same units.
    
    # Random intercepts
    (1 | Participant) + (1 | Word) +
    
    # In the random slopes below, `0 +` helps avoid redundant random intercepts
    # (background: https://github.com/lme4/lme4/issues/625)
    
    # By-participant random slopes
    (0 + z_word_cooccurrence | Participant) +
    (0 + z_visual_rating | Participant) +
    (0 + z_word_frequency | Participant) +
    (0 + z_orthographic_Levenshtein_distance | Participant) +
    (0 + z_word_concreteness | Participant) +
    
    # By-word random slopes
    (0 + z_vocabulary_size | Word) +
    (0 + z_recoded_participant_gender | Word) +
    (0 + z_information_uptake | Word),
  
  data = semanticdecision
)

# ############################################################################


# PRIOR PREDICTIVE CHECK WITH INFORMATIVE PRIORS

# Define priors

# In the function `set_prior` used below, the argument `class` often represents
# groups of parameters. For instance, class 'b' refers to fixed effects.

# Since most priors do not specify the negative/positive direction of effects,
# distributions are first specified for entire classes, and those are
# overridden wherever a direction is specified.

informative_priors =
  c(set_prior('normal(0, 0.1)', class = 'Intercept'),
    set_prior('normal(0, 0.1)', class = 'b'),
    set_prior('normal(0.1, 0.1)', class = 'b',
              coef = 'z_orthographic_Levenshtein_distance'),
    set_prior('normal(-0.1, 0.1)', class = 'b',
              coef = 'z_word_frequency'),
    set_prior('normal(0, 0.1)', class = 'sd'),  # automatically truncated to keep positive values only
    set_prior('normal(0, 0.1)', class = 'sigma')  # automatically truncated to keep positive values only
  )

# Perform a prior predictive check following Nicenboim et al. (2022, Section
# https://vasishth.github.io/bayescogsci/book/sec-ppd.html#sec:lognormal).

# Model

sampleonly_semanticdecision_brms_informativepriors =
  
  brm(
    
    # Dependent variable
    z_RTclean ~
      
      # FIXED EFFECTS
      
      # Controlled lexical variables
      z_word_frequency + z_orthographic_Levenshtein_distance +
      
      # Controlled word concreteness, important because the task is a word concreteness assessment.
      z_word_concreteness +
      
      # Control interaction between word concreteness and vocabulary size to allow a rigorous analysis of the
      # interaction between word co-occurrence and vocabulary size, and the interaction between word visual 
      # rating and vocabulary size.
      z_word_concreteness : z_vocabulary_size + 
      
      # Control interaction between word concreteness and participant gender to allow a rigorous analysis 
      # of the interaction between visual rating and participant gender.
      z_word_concreteness : z_recoded_participant_gender + 
      
      # Important control due to the relationship between this variable and vocabulary size (Pexman & Yap, 2018; 
      # http://germel.dyndns.org/psyling/pdf/2018_PexmanYap_SemCat_IndividualDifferences.pdf)
      z_information_uptake +
      
      # Control interactions with information uptake to allow a rigorous analysis of the interactions with 
      # vocabulary size specified below.
      z_word_cooccurrence : z_information_uptake +
      z_visual_rating : z_information_uptake +
      
      # Main effects of interest
      z_vocabulary_size + z_recoded_participant_gender + 
      z_word_cooccurrence + z_visual_rating +
      
      # Interactions of interest
      z_word_cooccurrence : z_vocabulary_size +
      z_visual_rating : z_vocabulary_size +
      z_word_cooccurrence : z_recoded_participant_gender +
      z_visual_rating : z_recoded_participant_gender + 
      
      # RANDOM EFFECTS: maximal structure constructed following the guidelines of Brauer and Curtin (2018;
      # https://psych.wisc.edu/Brauer/BrauerLab/wp-content/uploads/2014/04/Brauer-Curtin-2018-on-LMEMs.pdf).
      # Interactions only require random slopes if all variables involved vary within the same units.
      
      # Random intercepts
      (1 | Participant) + (1 | Word) +
      
      # In the random slopes below, `0 +` helps avoid redundant random intercepts 
      # (background: https://github.com/lme4/lme4/issues/625)
      
      # By-participant random slopes
      (0 + z_word_cooccurrence | Participant) + 
      (0 + z_visual_rating | Participant) +
      (0 + z_word_frequency | Participant) +
      (0 + z_orthographic_Levenshtein_distance | Participant) +
      (0 + z_word_concreteness | Participant) +
      
      # By-word random slopes
      (0 + z_vocabulary_size | Word) + 
      (0 + z_recoded_participant_gender | Word) +
      (0 + z_information_uptake | Word),
    
    data = semanticdecision, prior = informative_priors,
    
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

p1 = pp_check(sampleonly_semanticdecision_brms_informativepriors,
              type = 'stat', stat = 'min') +
  xlab('Response time (*z*)') +
  ggtitle('Prior predictive distribution of minimum values') +
  theme(axis.title.x = ggtext::element_markdown())

p2 = pp_check(sampleonly_semanticdecision_brms_informativepriors,
              type = 'stat', stat = 'mean') +
  xlab('Response time (*z*)') +
  ggtitle('Prior predictive distribution of means') +
  theme(axis.title.x = ggtext::element_markdown())

p3 = pp_check(sampleonly_semanticdecision_brms_informativepriors,
              type = 'stat', stat = 'max') +
  xlab('Response time (*z*)') +
  ggtitle('Prior predictive distribution of maximum values') +
  theme(axis.title.x = ggtext::element_markdown())

# Free up workspace
# rm(sampleonly_semanticdecision_brms_informativepriors)

p123 = p1 + p2 + p3 + plot_layout(ncol = 1)

ggsave(p123,
       filename = 'semanticdecision/bayesian_analysis/prior_predictive_checks/plots/semanticdecision_priorpredictivecheck_informativepriors.pdf',
       device = cairo_pdf, width = 7, height = 7, dpi = 900)

# Free up workspace
# rm(p1); rm(p2); rm(p3); rm(p123)


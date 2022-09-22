

# Part of Study 3: Lexical decision

# Part of Bayesian analysis

# Model with weakly-informative priors (SD = 0.2)


library(dplyr)
library(brms)  # Version 2.17.0

# Read in data
lexicaldecision = read.csv('lexicaldecision/data/final_dataset/lexicaldecision.csv')

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
              coef = 'z_orthographic_Levenshtein_distance'),
    set_prior('normal(-0.1, 0.2)', class = 'b',
              coef = 'z_word_frequency'),
    set_prior('normal(0, 0.2)', class = 'sd'),  # automatically truncated to keep positive values only
    set_prior('normal(0, 0.2)', class = 'sigma'),  # automatically truncated to keep positive values only
    set_prior('lkj(2)', class = 'cor')  # standard, regularising prior on random-effects covariance to aid convergence
  )

# Model

lexicaldecision_brms_weaklyinformativepriors_exgaussian =
  
  brm(
    
    # Dependent variable
    z_RT ~
      
      # FIXED EFFECTS
      
      # Controlled lexical variable
      z_orthographic_Levenshtein_distance +
      
      # Controlled semantic variable
      z_word_concreteness +
      
      # Control interactions with word concreteness to allow a rigorous analysis of
      # the interactions with visual rating specified futher below.
      z_word_concreteness : z_vocabulary_age +
      z_word_concreteness : z_recoded_participant_gender +
      
      # No covariate of vocabulary age (e.g., information uptake) was controlled for
      # because the online materials from the English Lexicon Project did not contain
      # such a covariate (materials hosted at https://elexicon.wustl.edu/ and
      # https://osf.io/n63s2/, accessed in June 2021).
      
      # Main effects of interest
      z_vocabulary_age + z_recoded_participant_gender +
      z_word_frequency + z_visual_rating +
      
      # Interactions of interest
      z_word_frequency : z_vocabulary_age +
      z_visual_rating : z_vocabulary_age +
      z_word_frequency : z_recoded_participant_gender +
      z_visual_rating : z_recoded_participant_gender +
      
      # RANDOM EFFECTS: maximal structure constructed following the guidelines of Brauer and Curtin (2018;
      # https://psych.wisc.edu/Brauer/BrauerLab/wp-content/uploads/2014/04/Brauer-Curtin-2018-on-LMEMs.pdf).
      # Interactions only require random slopes if all variables involved vary within the same units.
      
      # By-participant random slopes
      (z_word_frequency + z_visual_rating + 
         z_orthographic_Levenshtein_distance + 
         z_word_concreteness | Participant) +
      
      # By-word random slopes
      (z_vocabulary_age + z_recoded_participant_gender | word),
    
    data = lexicaldecision, prior = weaklyinformative_priors,
    
    # Following the prior predictive checks, an ex-gaussian distribution is set below.
    # This distribution is well suited for the analysis of response times (Bürkner et al., 
    # 2022, https://cran.r-project.org/web/packages/brms/brms.pdf; Rodríguez-Ferreiro 
    # et al., 2020, see supplementary materials via https://doi.org/10.7717/peerj.9511). 
    # Furthermore, the use of link functions, instead of transformations of the 
    # dependent variable, largely preserves the interpretability of the coefficients 
    # (Knief & Forstmeier, 2021; Lo & Andrews, 2015, https://doi.org/10.3389/fpsyg.2015.01171).
    
    family = exgaussian(),
    
    seed = 123,  # allow exact replication of results
    warmup = 2000,  # warmup iterations
    iter = 20000, chains = 5,  # (iter - warmup) x chains = total post-warmup draws (brms v2.17.0)
    cores = 5,  # parallel computation
    control = list(adapt_delta = 0.99, max_treedepth = 15)  # facilitate convergence
  )


# NOTE: Output takes up more than 30G, so it was removed from the project due to the 50G
# limit on OSF.io. If you wish to save the output, please uncomment the lines below.
# saveRDS(lexicaldecision_brms_weaklyinformativepriors_exgaussian,
#         'lexicaldecision/bayesian_analysis/results/lexicaldecision_brms_weaklyinformativepriors_exgaussian.rds')

# Model summary
summary(lexicaldecision_brms_weaklyinformativepriors_exgaussian) %>%
  saveRDS('lexicaldecision/bayesian_analysis/results/lexicaldecision_summary_weaklyinformativepriors_exgaussian.rds')

# Posterior predictive check
# Next, a posterior predictive check is performed to examine the sensitivity of 
# the model to priors varying in informativeness (for similar procedures, see 
# Rodríguez-Ferreiro et al., 2020, https://doi.org/10.7717/peerj.9511; 
# Stone et al., 2020, https://doi.org/10.7717/peerj.10438). The posterior 
# predictive checks from all models are compared in the folder 
# 'posterior_predictive_checks'.
pp_check(lexicaldecision_brms_weaklyinformativepriors_exgaussian, ndraws = 100) %>%
  saveRDS('lexicaldecision/bayesian_analysis/posterior_predictive_checks/lexicaldecision_posteriorpredictivecheck_weaklyinformativepriors_exgaussian.rds') 

# Plot posterior distribution of each fixed effect (i.e., effects commencing with 'b_')
mcmc_plot(lexicaldecision_brms_weaklyinformativepriors_exgaussian, 
          type = 'areas', variable = '^b_', regex = TRUE) %>%
  saveRDS('lexicaldecision/bayesian_analysis/results/posteriordistributions_semanticpriming_weaklyinformativepriors_exgaussian.rds')



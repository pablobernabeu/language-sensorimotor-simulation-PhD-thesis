

# Part of Study 1: Semantic priming

# Part of Bayesian analysis

# Model with informative priors (SD = 0.1)


library(dplyr)
library(brms)  # Version 2.17.0

# Read in data
semanticpriming = read.csv('semanticpriming/data/final_dataset/semanticpriming.csv')

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
              coef = 'z_target_number_syllables'),
    set_prior('normal(-0.1, 0.1)', class = 'b',
              coef = 'z_target_word_frequency'),
    set_prior('normal(0, 0.1)', class = 'sd'),  # automatically truncated to keep positive values only
    set_prior('normal(0, 0.1)', class = 'sigma'),  # automatically truncated to keep positive values only
    set_prior('lkj(2)', class = 'cor')  # standard, regularising prior on random-effects covariance to aid convergence
  )

# Model

semanticpriming_brms_informativepriors_exgaussian =
  
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
      
      # By-participant random slopes
      
      # Below, the random slopes for control covariates (i.e., 'z_target_word_frequency', 
      # 'z_target_number_syllables' and 'z_word_concreteness_diff') were removed due to 
      # non-convergence, inspired by Remedy 11 from Table 17 in Brauer and Curtin (2018). 
      # However, whereas Brauer and Curtin constrained such a removal to cases in which the 
      # covariate does not interact with any effects of interest, the random slopes for 
      # 'z_word_concreteness_diff' were removed below because the interactions between this 
      # covariate and the effects of interest were control covariates, not interactions 
      # of interest. That is, they were not critical to the research question.
    
    (z_cosine_similarity + z_visual_rating_diff + z_recoded_interstimulus_interval + 
       z_cosine_similarity : z_recoded_interstimulus_interval +
       z_visual_rating_diff : z_recoded_interstimulus_interval | Participant) +
      
      # Random slopes by prime-target pair
      
      # Below, the random slopes for the control covariate (i.e., 'z_attentional_control') 
      # were removed due to non-convergence, inspired by Remedy 11 from Table 17 in Brauer 
      # and Curtin (2018). However, whereas Brauer and Curtin constrained such a removal to 
      # cases in which the covariate does not interact with any effects of interest, the 
      # random slopes for 'z_attentional_control' were removed below because the interactions 
      # between this covariate and the effects of interest were control covariates, not 
      # interactions of interest. That is, they were not critical to the research question.
      
    (z_vocabulary_size + z_recoded_participant_gender | primeword_targetword),
    
    data = semanticpriming, prior = informative_priors,
    
    # Following the prior predictive checks, an ex-gaussian distribution is set below.
    # This distribution is well suited for the analysis of response times (Bürkner et al., 
    # 2022, https://cran.r-project.org/web/packages/brms/brms.pdf; Rodríguez-Ferreiro 
    # et al., 2020, see supplementary materials via https://doi.org/10.7717/peerj.9511). 
    # Furthermore, the use of link functions, instead of transformations of the 
    # dependent variable, largely preserves the interpretability of the coefficients 
    # (Knief & Forstmeier, 2021; Lo & Andrews, 2015, https://doi.org/10.3389/fpsyg.2015.01171).
    
    family = exgaussian(),
    
    seed = 123,  # allow exact replication of results
    warmup = 1500,  # warmup iterations
    iter = 9000, chains = 16,  # (iter - warmup) x chains = total post-warmup draws (brms v2.17.0)
    cores = 16,  # parallel computation
    control = list(adapt_delta = 0.99, max_treedepth = 15)  # facilitate convergence
  )


# NOTE: Output takes up more than 15G, so it was removed from the project due to the 50G
# limit on OSF.io. If you wish to save the output, please uncomment the lines below.
# saveRDS(semanticpriming_brms_informativepriors_exgaussian,
#         'semanticpriming/bayesian_analysis/results/semanticpriming_brms_informativepriors_exgaussian.rds')

# Model summary
summary(semanticpriming_brms_informativepriors_exgaussian) %>%
  saveRDS('semanticpriming/bayesian_analysis/results/semanticpriming_summary_informativepriors_exgaussian.rds')

# Posterior predictive check
# Next, a posterior predictive check is performed to examine the sensitivity of 
# the model to priors varying in informativeness (for similar procedures, see 
# Rodríguez-Ferreiro et al., 2020, https://doi.org/10.7717/peerj.9511; 
# Stone et al., 2020, https://doi.org/10.7717/peerj.10438). The posterior 
# predictive checks from all models are compared in the folder 
# 'posterior_predictive_checks'.
pp_check(semanticpriming_brms_informativepriors_exgaussian, ndraws = 100) %>%
  saveRDS('semanticpriming/bayesian_analysis/posterior_predictive_checks/semanticpriming_posteriorpredictivecheck_informativepriors_exgaussian.rds') 

# Plot posterior distribution of each fixed effect (i.e., effects commencing with 'b_')
mcmc_plot(semanticpriming_brms_informativepriors_exgaussian, 
          type = 'areas', variable = '^b_', regex = TRUE) %>%
  saveRDS('semanticpriming/bayesian_analysis/results/posteriordistributions_semanticpriming_informativepriors_exgaussian.rds')



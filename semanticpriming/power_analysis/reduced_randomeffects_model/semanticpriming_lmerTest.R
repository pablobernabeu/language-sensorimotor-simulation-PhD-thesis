

# Part of Study 1: Semantic priming

# Deprecated model. For details, please see README.md in the directory
# 'semanticpriming/power_analysis/reduced_randomeffects_model'

library(dplyr)  # data wrangling
# The following lme4-relevant package was installed before lme4 to avoid a conflict 
# noted at https://cran.r-project.org/web/packages/lme4/lme4.pdf
library(RcppEigen)
library(lme4)   # Mixed-effects models (package version 1.1-26)
library(lmerTest)  # Compute p values (package version 3.1-3)
library(MuMIn)   # R^2

# Read in data
semanticpriming = read.csv('semanticpriming/data/final_dataset/semanticpriming.csv')

# MODEL
# Measure running time
system.time({
  
  # Model name
  semanticpriming_lmerTest =
    
    lmerTest::lmer( 
      
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
        # Double vertical bars (||) are used to remove correlations among random effects, with the aim of
        # aiding the convergence of the model (i.e., Remedy 15 in Table 17 of Brauer & Curtin, 2018).
        
        # Random intercepts
        (1 | Participant) + (1 | target_word) +
        
        # In the random slopes below, the prefix `0 +` helps avoid redundant random intercepts 
        # (see https://github.com/lme4/lme4/issues/625) and reduces the random-effects
        # structure (Brauer & Curtin, 2018).
        
        # By-participant random slopes
        (0 + z_cosine_similarity || Participant) + 
        (0 + z_visual_rating_diff || Participant) +
        (0 + z_recoded_interstimulus_interval || Participant) +
        (0 + z_cosine_similarity : z_recoded_interstimulus_interval || Participant) +
        (0 + z_visual_rating_diff : z_recoded_interstimulus_interval || Participant) +
        # Random slopes below removed due to non-convergence, following 
        # Remedy 11 from Table 17 in Brauer and Curtin (2018).
        # (0 + z_target_word_frequency || Participant) + 
        # (0 + z_target_number_syllables || Participant) +
        # (0 + z_word_concreteness_diff || Participant) +
        
        # Random slopes by prime-target pair
        (0 + z_vocabulary_size || target_word) + 
        (0 + z_recoded_participant_gender || target_word),
      # Random slopes below removed due to non-convergence, following 
      # Remedy 11 from Table 17 in Brauer and Curtin (2018).
      # (0 + z_attentional_control || target_word),
      
      data = semanticpriming,
      
      # Set maximum number of iterations to 1m to facilitate convergence 
      # (Brauer & Curtin, 2018; Singmann & Kellen, 2019)
      control = lmerControl(optCtrl = list(maxfun = 1e6))
    )
})

saveRDS(semanticpriming_lmerTest, 
        'semanticpriming/power_analysis/reduced_randomeffects_model/results/semanticpriming_lmerTest.rds')

# Calculate p values using Kenward-Roger method (Luke, 2017; 
# https://doi.org/10.3758/s13428-016-0809-y)
summary(semanticpriming_lmerTest, ddf = 'Kenward-Roger') %>%
  saveRDS('semanticpriming/power_analysis/reduced_randomeffects_model/results/KR_summary_semanticpriming_lmerTest.rds')

# Calculate R^2. The result must be interpreted with caution as it differs from the 
# traditional R^2 (Nakagawa et al., 2017; https://doi.org/10.1098/rsif.2017.0213)
MuMIn::r.squaredGLMM(semanticpriming_lmerTest) %>%
  saveRDS('semanticpriming/power_analysis/reduced_randomeffects_model/results/Nakagawa2017_R2_semanticpriming_lmerTest.rds')

# Calculate 95% confidence intervals of fixed effects
lme4::confint.merMod(semanticpriming_lmerTest, method = 'profile',
                     # Compute 95% CIs for every effect, as well as for the intercept
                     parm = rownames(summary(semanticpriming_lmerTest)$coefficients)) %>%
  saveRDS('semanticpriming/power_analysis/reduced_randomeffects_model/results/confint_semanticpriming_lmerTest.rds')

# Save random effects
lme4::ranef(semanticpriming_lmerTest) %>%
  saveRDS('semanticpriming/power_analysis/reduced_randomeffects_model/results/ranef_semanticpriming_lmerTest.rds')



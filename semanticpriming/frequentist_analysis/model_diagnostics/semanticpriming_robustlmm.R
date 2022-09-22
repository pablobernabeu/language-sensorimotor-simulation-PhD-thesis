

# Part of Study 1: Semantic priming

# This {robustlmm} model is used to validate the results of the standard mixed-effects model, 
# in which the residuals were not normally distributed. The robustlmm package detects and 
# removes outliers that hinder the normality of the residuals.

# Package paper: Koller (2016), http://dx.doi.org/10.18637/jss.v075.i06

# Use case: Sleegers et al. (2020), https://doi.org/10.1177/1948550620966153

# Further background:
# Knief and Forstmeier (2021), https://doi.org/10.1101/498931
# Schielzeth et al. (2020), https://doi.org/10.1111/2041-210X.13434


# install.packages('robustlmm', repos = 'https://www.stats.bris.ac.uk/R/')
library(robustlmm)

# Read in data
semanticpriming = read.csv('semanticpriming/data/final_dataset/semanticpriming.csv')

# Model
semanticpriming_robustlmm = 
  
  robustlmm::rlmer(
    
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
      (1 | Participant) + (1 | target_word) +
      
      # In the random slopes below, `0 +` helps avoid redundant random intercepts 
      # (background: https://github.com/lme4/lme4/issues/625)
      
      # By-participant random slopes
      (0 + z_cosine_similarity || Participant) + 
      (0 + z_visual_rating_diff || Participant) +
      (0 + z_recoded_interstimulus_interval || Participant) +
      (0 + z_cosine_similarity : z_recoded_interstimulus_interval || Participant) +
      (0 + z_visual_rating_diff : z_recoded_interstimulus_interval || Participant) +
      # Random slopes below removed due to non-convergence, inspired by Remedy 11 from Table 17 
      # in Brauer and Curtin (2018).
      # (0 + z_target_word_frequency || Participant) + 
      # (0 + z_target_number_syllables || Participant) +
      # (0 + z_word_concreteness_diff || Participant) +
      
      # Random slopes by prime-target pair
      (0 + z_vocabulary_size || target_word) + 
      (0 + z_recoded_participant_gender || target_word),
    # Random slopes below removed due to non-convergence, inspired by Remedy 11 from Table 17 
    # in Brauer and Curtin (2018).
    # (0 + z_attentional_control || target_word),
    
    data = semanticpriming,
    
    # Set maximum iterations to 1m to facilitate convergence 
    # (Brauer & Curtin, 2018; Singmann & Kellen, 2019)
    control = lmerControl(optCtrl = list(maxfun = 1e6))
  )


#############
# 
# The above model could not finish running due to the following error:
# 
# Error in .local(x, y, ...) : negative length vectors are not allowed
# Calls: <Anonymous> ... <Anonymous> -> tcrossprod -> tcrossprod -> .local -> .Call
# 
# As a result, the code below failed too.
# 
#############


# Calculate p values using Kenward-Roger approximation for degrees of freedom 
# (Luke, 2017, https://doi.org/10.3758/s13428-016-0809-y), similarly to 
# Sleegers et al. (2020; https://doi.org/10.1177/1948550620966153).

# Get coefficients from non-robust model to extract Satterthwaite-approximated df
KR_summary_semanticpriming_lmerTest = 
  readRDS("semanticpriming/frequentist_analysis/results/KR_summary_semanticpriming_lmerTest.rds")
coefs_lmerTest = data.frame(coef(KR_summary_semanticpriming_lmerTest))

# Get coefficients from robust model to extract t values
semanticpriming_robustlmm = data.frame(coef(summary(semanticpriming_robustlmm)))

# Calculate p values based on robust t values and non-robust approx. df
semanticpriming_robustlmm$p = 2 * pt(abs(semanticpriming_robustlmm[, 't.value']), 
                                     coefs_lmerTest$df, lower = FALSE)

# Save
saveRDS(semanticpriming_robustlmm, 
        'semanticpriming/frequentist_analysis/model_diagnostics/results/semanticpriming_robustlmm.rds')



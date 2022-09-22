

# Part of Study 3: Lexical decision

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
lexicaldecision = read.csv('lexicaldecision/data/final_dataset/lexicaldecision.csv')

# Model
lexicaldecision_robustlmm =
  
  robustlmm::rlmer(z_RT ~
                     
                     # FIXED EFFECTS
                     
                     # Controlled lexical variables
                     z_orthographic_Levenshtein_distance +
                     
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
                     # Interaction effects only require random slopes if all interacting variables vary within the same units.
                     
                     # Random intercepts
                     (1 | Participant) + (1 | word) +
                     
                     # In the random slopes below, `0 +` helps avoid redundant random intercepts 
                     # (background: https://github.com/lme4/lme4/issues/625)
                     
                     # By-participant random slopes
                     (0 + z_word_frequency || Participant) + 
                     (0 + z_visual_rating || Participant) +
                     # Random slopes below removed due to non-convergence, following 
                     # Remedy 11 from Table 17 in Brauer and Curtin (2018).
                     # (0 + z_orthographic_Levenshtein_distance || Participant) +
                     
                     # By-word random slopes
                     (0 + z_vocabulary_age || word) + 
                     (0 + z_recoded_participant_gender || word),
                   
                   data = lexicaldecision,
                   
                   # Set maximum iterations to 1m to facilitate convergence 
                   # (Brauer & Curtin, 2018; Singmann & Kellen, 2019)
                   control = lmerControl(optCtrl = list(maxfun = 1e6)))


#############
# 
# The above model could not finish running due to the following error:
# 
# Error in h(simpleError(msg, call)) : 
#   error in evaluating the argument 'y' in selecting a method for function 'crossprod': Cholmod error 'out of memory' at file ../Core/cholmod_memory.c, line 146
# Calls: <Anonymous> ... eval -> eval -> as -> asMethod -> .handleSimpleError -> h
# 
# As a result, the code below failed too.
# 
#############


# Calculate p values using Kenward-Roger approximation for degrees of freedom 
# (Luke, 2017, https://doi.org/10.3758/s13428-016-0809-y), similarly to 
# Sleegers et al. (2020; https://doi.org/10.1177/1948550620966153).

# Get coefficients from non-robust model to extract Satterthwaite-approximated df
KR_summary_lexicaldecision_lmerTest = 
  readRDS("lexicaldecision/frequentist_analysis/results/KR_summary_lexicaldecision_lmerTest.rds")
coefs_lmerTest = data.frame(coef(KR_summary_lexicaldecision_lmerTest))

# Get coefficients from robust model to extract t values
lexicaldecision_robustlmm = data.frame(coef(summary(lexicaldecision_robustlmm)))

# Calculate p values based on robust t values and non-robust approx. df
lexicaldecision_robustlmm$p = 2 * pt(abs(lexicaldecision_robustlmm[, 't.value']), 
                                     coefs_lmerTest$df, lower = FALSE)

# Save
saveRDS(lexicaldecision_robustlmm,
        'lexicaldecision/frequentist_analysis/model_diagnostics/results/lexicaldecision_robustlmm.rds')




# Part of Study 2: Semantic decision

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
semanticdecision = read.csv('semanticdecision/data/final_dataset/semanticdecision.csv')

# Model
semanticdecision_robustlmm =
  
  robustlmm::rlmer(z_RTclean ~
                     
                     # FIXED EFFECTS
                     
                     # Controlled lexical variables
                     z_word_frequency + z_orthographic_Levenshtein_distance +
                     
                     # Controlled word concreteness, important because the task is a word concreteness assessment.
                     z_word_concreteness +
                     
                     # Important control due to the relationship between this variable and vocabulary size (Pexman & Yap, 2018; 
                     # http://germel.dyndns.org/psyling/pdf/2018_PexmanYap_SemCat_IndividualDifferences.pdf)
                     z_information_uptake +
                     
                     # Control interactions with information uptake to allow a rigorous analysis of the interactions with 
                     # vocabulary size specified below.
                     z_word_cooccurrence : z_information_uptake +
                     z_visual_rating : z_information_uptake +
                     
                     # Control interaction between word concreteness and vocabulary size to allow a rigorous analysis of the
                     # interaction between word co-occurrence and vocabulary size, and the interaction between word visual 
                     # rating and vocabulary size).
                     z_word_concreteness : z_vocabulary_size + 
                     
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
                     (0 + z_word_cooccurrence || Participant) + 
                     (0 + z_visual_rating || Participant) +
                     # Random slopes below removed due to non-convergence, following 
                     # Remedy 11 from Table 17 in Brauer and Curtin (2018).
                     # (0 + z_word_frequency || Participant) + 
                     # (0 + z_orthographic_Levenshtein_distance || Participant) +
                     # (0 + z_word_concreteness || Participant) +
                     
                     # By-word random slopes
                     (0 + z_vocabulary_size || Word) + 
                     (0 + z_recoded_participant_gender || Word),
                   # Random slopes below removed due to non-convergence, following 
                   # Remedy 11 from Table 17 in Brauer and Curtin (2018).
                   # (0 + z_information_uptake || Word),
                   
                   data = semanticdecision,
                   
                   # Set maximum iterations to 1m to facilitate convergence 
                   # (Brauer & Curtin, 2018; Singmann & Kellen, 2019)
                   control = lmerControl(optCtrl = list(maxfun = 1e6)))


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
KR_summary_semanticdecision_lmerTest = 
  readRDS("semanticdecision/frequentist_analysis/results/KR_summary_semanticdecision_lmerTest.rds")
coefs_lmerTest = data.frame(coef(KR_summary_semanticdecision_lmerTest))

# Get coefficients from robust model to extract t values
semanticdecision_robustlmm = data.frame(coef(summary(semanticdecision_robustlmm)))

# Calculate p values based on robust t values and non-robust approx. df
semanticdecision_robustlmm$p = 2 * pt(abs(semanticdecision_robustlmm[, 't.value']), 
                                      coefs_lmerTest$df, lower = FALSE)

# Save
saveRDS(semanticdecision_robustlmm, 
        'semanticdecision/frequentist_analysis/model_diagnostics/results/semanticdecision_robustlmm.rds')



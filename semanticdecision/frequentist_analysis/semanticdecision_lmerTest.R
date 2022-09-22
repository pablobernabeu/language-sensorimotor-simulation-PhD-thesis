

# Part of Study 2: Semantic decision

library(dplyr)  # data wrangling
# The following lme4-relevant package was installed before lme4 to avoid a conflict 
# noted at https://cran.r-project.org/web/packages/lme4/lme4.pdf
library(RcppEigen)
library(lme4)   # Mixed-effects models (package version 1.1-26)
library(lmerTest)  # Compute p values (package version 3.1-3)
library(MuMIn)   # R^2

# Read in data
semanticdecision = read.csv('semanticdecision/data/final_dataset/semanticdecision.csv')

# MODEL
# Measure running time
system.time({
  
  # Model name
  semanticdecision_lmerTest =
    
    lmerTest::lmer( 
      
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
        # Double vertical bars (||) are used to remove correlations among random effects, with the aim of
        # aiding the convergence of the model (i.e., Remedy 15 in Table 17 of Brauer & Curtin, 2018).
        
        # Random intercepts
        (1 | Participant) + (1 | Word) +
        
        # In the random slopes below, the prefix `0 +` helps avoid redundant random intercepts 
        # (see https://github.com/lme4/lme4/issues/625) and reduces the random-effects
        # structure (Brauer & Curtin, 2018).
        
        # By-participant random slopes
        (0 + z_word_cooccurrence || Participant) + 
        (0 + z_visual_rating || Participant) +
        
        # Random slopes below removed due to non-convergence, inspired by Remedy 11 from Table 17 
        # in Brauer and Curtin (2018). However, whereas Brauer and Curtin constrained such a 
        # removal to cases in which the covariate does not interact with any effects of interest,
        # the random slopes for 'z_word_concreteness' are removed below because the interactions 
        # between this covariate and the effects of interest are control covariates per se, 
        # not interactions of interest. That is, they are not critical to the research question.
        # (0 + z_word_frequency || Participant) + 
        # (0 + z_orthographic_Levenshtein_distance || Participant) +
        # (0 + z_word_concreteness || Participant) +
        
      # By-word random slopes
      (0 + z_vocabulary_size || Word) + 
        (0 + z_recoded_participant_gender || Word),
      
      # Random slopes below removed due to non-convergence, inspired by Remedy 11 from Table 17 
      # in Brauer and Curtin (2018). However, whereas Brauer and Curtin constrained such a 
      # removal to cases in which the covariate does not interact with any effects of interest,
      # the random slopes for 'z_information_uptake' are removed below because the interactions 
      # between this covariate and the effects of interest are control covariates per se, 
      # not interactions of interest. That is, they are not critical to the research question.
      # (0 + z_information_uptake || Word),
      
      data = semanticdecision,
      
      # Set maximum number of iterations to 1m to facilitate convergence 
      # (Brauer & Curtin, 2018; Singmann & Kellen, 2019)
      control = lmerControl(optCtrl = list(maxfun = 1e6))
    )
})

saveRDS(semanticdecision_lmerTest, 
        'semanticdecision/frequentist_analysis/results/semanticdecision_lmerTest.rds')

# Calculate p values using Kenward-Roger method (Luke, 2017; 
# https://doi.org/10.3758/s13428-016-0809-y)
summary(semanticdecision_lmerTest, ddf = 'Kenward-Roger') %>%
  saveRDS('semanticdecision/frequentist_analysis/results/KR_summary_semanticdecision_lmerTest.rds')

# Calculate R^2. The result must be interpreted with caution as it differs from the 
# traditional R^2 (Nakagawa et al., 2017; https://doi.org/10.1098/rsif.2017.0213)
MuMIn::r.squaredGLMM(semanticdecision_lmerTest) %>%
  saveRDS('semanticdecision/frequentist_analysis/results/Nakagawa2017_R2_semanticdecision_lmerTest.rds')

# Calculate 95% confidence intervals of fixed effects
lme4::confint.merMod(semanticdecision_lmerTest, method = 'profile',
                     # Compute 95% CIs for every effect, as well as for the intercept
                     parm = rownames(summary(semanticdecision_lmerTest)$coefficients)) %>%
  saveRDS('semanticdecision/frequentist_analysis/results/confint_semanticdecision_lmerTest.rds')

# Save random effects
lme4::ranef(semanticdecision_lmerTest) %>%
  saveRDS('semanticdecision/frequentist_analysis/results/ranef_semanticdecision_lmerTest.rds')



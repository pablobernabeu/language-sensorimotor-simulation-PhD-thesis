

# Part of Study 1: Semantic priming

# To resolve the collinearity between several lexical variables, select the most influential variable
# in each set of collinear variables (Harrison et al., 2018; https://peerj.com/articles/4794/).

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
  
  lexical_covariates_selection_semanticpriming_lmerTest =
    
    lmerTest::lmer(
      
      # Dependent variable
      z_target.RT ~
        
        # FIXED EFFECTS
        z_target_word_frequency + 
        z_target_length + 
        z_target_number_syllables + 
        z_target_orthographic_Levenshtein_distance + 
        z_target_phonological_Levenshtein_distance +
        
        # RANDOM EFFECTS: maximal structure constructed following the guidelines of Brauer and Curtin (2018; 
        # https://psych.wisc.edu/Brauer/BrauerLab/wp-content/uploads/2014/04/Brauer-Curtin-2018-on-LMEMs.pdf).
        # Interactions only require random slopes if all variables involved vary within the same units.
        # Double vertical bars (||) are used to remove correlations among random effects, with the aim of
        # aiding the convergence of the model (i.e., Remedy 15 in Table 17 of Brauer & Curtin, 2018).
        
        # Random intercepts
        (1 | Participant) + (1 | target_word) +
        
        # In the random slopes below, the prefix `0 +` helps avoid redundant random 
        # intercepts (see https://github.com/lme4/lme4/issues/625)
        
        # Random slopes
        (0 + z_target_word_frequency || Participant) + 
        (0 + z_target_length || Participant) +
        (0 + z_target_number_syllables || Participant) +
        (0 + z_target_orthographic_Levenshtein_distance || Participant) +
        (0 + z_target_phonological_Levenshtein_distance || Participant),
      
      data = semanticpriming,
      
      # Set maximum iterations to 1m to facilitate convergence 
      # (Brauer & Curtin, 2018; Singmann & Kellen, 2019)
      control = lmerControl(optCtrl = list(maxfun = 1e6))
    )
})

saveRDS(lexical_covariates_selection_semanticpriming_lmerTest, 
        'semanticpriming/frequentist_analysis/lexical_covariates_selection/results/lexical_covariates_selection_semanticpriming_lmerTest.rds')

# Calculate p values using Kenward-Roger method (Luke, 2017; 
# https://doi.org/10.3758/s13428-016-0809-y)
summary(lexical_covariates_selection_semanticpriming_lmerTest, ddf = 'Kenward-Roger') %>%
  saveRDS('semanticpriming/frequentist_analysis/lexical_covariates_selection/results/KR_summary_lexical_covariates_selection_semanticpriming_lmerTest.rds')

# Calculate R^2. The result must be interpreted with caution as it differs from the 
# traditional R^2 (Nakagawa et al., 2017; https://doi.org/10.1098/rsif.2017.0213)
MuMIn::r.squaredGLMM(lexical_covariates_selection_semanticpriming_lmerTest) %>%
  saveRDS('semanticpriming/frequentist_analysis/lexical_covariates_selection/results/Nakagawa2017_R2_lexical_covariates_selection_semanticpriming_lmerTest.rds')

# Calculate 95% confidence intervals of fixed effects
lme4::confint.merMod(lexical_covariates_selection_semanticpriming_lmerTest, method = 'profile',
                     # Compute 95% CIs for every effect, as well as for the intercept
                     parm = rownames(summary(lexical_covariates_selection_semanticpriming_lmerTest)$coefficients)) %>%
  saveRDS('semanticpriming/frequentist_analysis/lexical_covariates_selection/results/confint_lexical_covariates_selection_semanticpriming_lmerTest.rds')



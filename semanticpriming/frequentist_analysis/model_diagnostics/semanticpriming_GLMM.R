

# Part of Study 1: Semantic priming

# Generalized linear mixed-effects model motivated by the non-normal distribution of the residuals 
# from the linear mixed-effects model (Lo & Andrews, 2015). The dependent variable used in this 
# model was raw RT, rather than standardized, RT, as none of the families of distributions tested 
# by Lo and Andrews (2015) allow values below zero (see Table 15.2 in Fox, 2015). We used the 
# Inverse Gaussian family with an identity link, which was the family that yielded the most normal 
# residuals in the analysis by Lo and Andrews.
# 
# P values were calculated through parametric bootstrapping, which is the most robust method for 
# a GLMM, as the Kenward-Roger and Satterthwaite methods are not possible for these models (Luke, 
# 2017; Singmann et al., 2021).

# Further background:
# Knief and Forstmeier (2021), https://doi.org/10.1101/498931
# Schielzeth et al. (2020), https://doi.org/10.1111/2041-210X.13434
# 
# References
# 
# Fox, J. (2016). Generalized Linear Models. In Applied regression analysis and generalized 
# linear models (Third Edition, pp. 418–472). SAGE.
# 
# Lo, S, & Andrews, S. (2015). To transform or not to transform: Using generalized linear mixed 
# models to analyse reaction time data. Frontiers in Psychology, 6, 1171. 
# https://doi.org/10.3389/fpsyg.2015.01171
# 
# Luke, S. G. (2017). Evaluating significance in linear mixed-effects models in R. Behavior 
# Research Methods, 49(4), 1494--1502. https://doi.org/10.3758/s13428-016-0809-y
# 
# Singmann, H., Bolker, B., Westfall, J., Aust, F., &  Ben-Sachar, M. S. (2021). Package 'afex'. 
# CRAN. https://cran.r-project.org/web/packages/afex/afex.pdf


# install.packages('afex', repos = 'https://www.stats.bris.ac.uk/R/')
library(afex)  # Compute p values (package version 3.1-3) using parametric bootstrapping (see information below)
library(parallel)  # Parallel processing for a faster computation (included in base R)

# Read in data
semanticpriming = read.csv('semanticpriming/data/final_dataset/semanticpriming.csv')

# Enable parallel clusters, which allows faster processing if several cores available.
nc = detectCores() # number of cores

# Create cluster. Track progress through output file:
cl = makeCluster(rep("localhost", nc), outfile = "semanticpriming_GLMM.cl.log.txt")

# Model 
afex::mixed( 
  
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
  
  # Method from Lo and Andrews (2015; https://doi.org/10.3389/fpsyg.2015.01171)
  family = inverse.gaussian(link = 'identity'),
  
  # Set maximum iterations to 1m to facilitate convergence 
  # (Brauer & Curtin, 2018; Singmann & Kellen, 2019)
  control = glmerControl(optCtrl = list(maxfun = 1e6)),
  
  # Parametric bootstrapping (PB), most robust method available for GLMM (Kenward-Roger and Satterthwaite 
  # are not available for GLMM; see Luke, 2017, and https://cran.r-project.org/web/packages/afex/afex.pdf)
  method = 'PB',
  
  # Number of parametric bootstrapping iterations, run in parallel (`cl`)
  args_test = list(nsim = 500, cl = cl),
  
  # Enable parallel computation
  cl = cl
  
) %>%
  
  saveRDS('semanticpriming/frequentist_analysis/model_diagnostics/results/semanticpriming_GLMM.rds')


#############
# 
# The above model could not finish running due to the following error:
# 
# Error in checkForRemoteErrors(val) : 
#   17 nodes produced errors; first error: cannot open file '/mmfs1/home/users/dejuanbe/R/x86_64-pc-linux-gnu-library/4.0/lme4/R/sysdata.rdb': No such file or directory
# Calls: system.time ... clusterApplyLB -> dynamicClusterApply -> checkForRemoteErrors
# 
#############





# Part of Study 3: Lexical decision

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
lexicaldecision = read.csv('lexicaldecision/data/final_dataset/lexicaldecision.csv')

# Enable parallel clusters, which allows faster processing if several cores available.
nc = detectCores() # number of cores

# Create cluster. Track progress through output file:
cl = makeCluster(rep("localhost", nc), outfile = "lexicaldecision_GLMM.cl.log.txt")

# Model 
afex::mixed( RT ~
               
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
  
  saveRDS('semanticpriming/frequentist_analysis/model_diagnostics/results/lexicaldecision_GLMM.rds')


#############
# 
# The above model could not finish running due to the following error:
# 
# Error in checkForRemoteErrors(val) : 
#   10 nodes produced errors; first error: (maxstephalfit) PIRLS step-halvings failed to reduce deviance in pwrssUpdate
# Calls: system.time ... clusterApplyLB -> dynamicClusterApply -> checkForRemoteErrors
# 
#############


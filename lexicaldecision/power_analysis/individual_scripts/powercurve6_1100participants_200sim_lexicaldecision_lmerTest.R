

# Part of Study 1: Lexical decision

# install.packages('simr', repos = 'https://www.stats.bris.ac.uk/R/') 
library(simr)    # power curve (package version 1.0.5)

# Read in data and model

lexicaldecision = 
  read.csv('lexicaldecision/data/final_dataset/lexicaldecision.csv')

lexicaldecision_lmerTest = 
  readRDS('lexicaldecision/frequentist_analysis/results/lexicaldecision_lmerTest.rds')

# For the power curve, reduce effect size by 20% to account for various differences between 
# the pilot and the study to be conducted, as well as to account for publication bias. See:
# Brysbaert and Stevens (2018) -- https://doi.org/10.5334/joc.10;
# Green and MacLeod (2016) -- https://doi.org/10.1111/2041-210X.12504;
# Kumle et al. (2021) -- https://doi.org/10.3758/s13428-021-01546-0;
# Loken and Gelman (2017) -- https://doi.org/10.1126/science.aal3618

# To reduce effect size by 20%, multiply it by 0.8.

fixef(lexicaldecision_lmerTest)['z_recoded_participant_gender:z_word_frequency'] = 
  fixef(lexicaldecision_lmerTest)['z_recoded_participant_gender:z_word_frequency'] * 0.8

# To allow the estimation of power for various sample sizes, first extend the current maximum sample size
# (for a description of these steps, see Green & MacLeod, 2016, https://doi.org/10.1111/2041-210X.12504).
# Further below, in the function `powerCurve`, the `breaks` parameter sets the specific sample size tested.
# Note that many more sample sizes were examined in parallel, which are specified in other scripts. The
# upper directory ('power_analysis') contains power-curve plots that display all the sample sizes.

extendedparticipants_lexicaldecision_lmerTest = 
  extend(lexicaldecision_lmerTest, along = 'Participant', n = 2000)

# Run power curve (note that it takes around a month to complete). If necessary, the process can be sped up
# by changing the `method` parameter of `fixed` to `z` (i.e., t-as-z approximation), but note that this is
# less conservative (Luke, 2017, https://doi.org/10.3758/s13428-016-0809-y). Another option is to decrease
# the number of simulations through the `nsim` parameter, although this would increase the variance.

powercurve6 = powerCurve(extendedparticipants_lexicaldecision_lmerTest,
 
                         # Calculate power for the fixed effect specified below. The Satterthwaite method ('sa')  
                         # is used for the calculation of p values. This method is robust for large sample
                         # sizes, while incurring a bit less time than the Kenward-Roger method (Luke, 2017).

                         fixed('z_recoded_participant_gender:z_word_frequency', method = 'sa'), 

                         # for the following number of participants
                         along = 'Participant', breaks = 1100,

                         # using this number of simulations
                         nsim = 200,
 
                         # with the following seed number to allow the reproducibility of the results
                         seed = 123)

saveRDS(powercurve6, 
  'lexicaldecision/power_analysis/results/powercurve6_1100participants_200sim_lexicaldecision_lmerTest.rds')





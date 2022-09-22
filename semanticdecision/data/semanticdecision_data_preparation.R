

# Part of Study 2: Semantic decision

# Preparation of final data set by merging several data sets from previous studies.

# All primary data sets were last downloaded on 2 January 2022.


# install.packages('dplyr')         # Data wrangling
# install.packages('httr')          # Data download
# install.packages('readxl')        # Data reading
# install.packages('standardize')   # Used to cluster-mean center within-participants variables 

library(httr)
library(readxl)
library(dplyr)
library(standardize)


# DATA SET 1. Calgary Semantic Decision data (Pexman et al., 2017), downloaded from 'Electronic supplementary material' 
# at https://doi.org/10.3758/s13428-016-0720-6. Download URL:
# https://static-content.springer.com/esm/art%3A10.3758%2Fs13428-016-0720-6/MediaObjects/13428_2016_720_MOESM3_ESM.xlsx

# Note on reproducibility
# The code below was used to download and save the data set in the folder 'semanticdecision/data/primary_datasets'. 
# To prevent any influence from future changes to the original data set online, the code below was protected (i.e., 
# commented out) by inserting the snippet `# Protected code # ` at the beginning of each line. If desired, the code
# can be run by removing the protective snippet from every line.

# Protected code # GET('https://static-content.springer.com/esm/art%3A10.3758%2Fs13428-016-0720-6/MediaObjects/13428_2016_720_MOESM3_ESM.xlsx', 
# Protected code #     write_disk(tf <- tempfile(fileext = ".xlsx")))
# Protected code # read_excel(tf) %>%
# Protected code #   write.csv('semanticdecision/data/primary_datasets/Pexman_etal_2017_semanticdecision.csv', 
# Protected code #             row.names = FALSE)

# Read in data set
semanticdecision =
  read.csv('semanticdecision/data/primary_datasets/Pexman_etal_2017_semanticdecision.csv')

str(semanticdecision)
semanticdecision$RTclean = 
  as.numeric(as.character(semanticdecision$RTclean))

# Descriptives
head(semanticdecision)
nrow(semanticdecision)
length(unique(semanticdecision$Participant))
length(unique(semanticdecision$Word))

# Rename vocabulary size
semanticdecision = semanticdecision %>% rename(vocabulary_size = NAART)

# Retain relevant variables. The variable 'RTclean' (Pexman et al., 2017) is the result of removing trials 
# with an incorrect response, as well as trials with a response faster than 250 ms or slower than 3,000 ms 
# (i.e., the trial time-out).
semanticdecision = 
  semanticdecision[
    complete.cases(semanticdecision$RTclean), 
    c('Participant', 'Word', 'TrialBlock' , 'vocabulary_size', 'RTclean')]


# DATA SET 2. Individual differences (Pexman & Yap, 2018; https://doi.org/10.1037/xlm0000499), 
# downloaded from https://osf.io/uyj7m/?version=1

# Note on reproducibility
# The code below was used to download and save the data set in the folder 'semanticdecision/data/primary_datasets'. 
# To prevent any influence from future changes to the original data set online, the code below was protected (i.e., 
# commented out) by inserting the snippet `# Protected code # ` at the beginning of each line. If desired, the code
# can be run by removing the protective snippet from every line.

# Protected code # GET('https://osf.io/uyj7m/download?version=1',
# Protected code #     write_disk(tf <- tempfile(fileext = ".xlsx")))
# Protected code # read_excel(tf) %>%
# Protected code #   write.csv('semanticdecision/data/primary_datasets/Pexman_Yap_2018_individualdifferences.csv', 
# Protected code #             row.names = FALSE)

# Read in data set
PexmanYap2018_individual = 
  read.csv('semanticdecision/data/primary_datasets/Pexman_Yap_2018_individualdifferences.csv')

# Information uptake
# The variable 'information_uptake' created below will be used as a covariate of vocabulary size 
# (see Pexman & Yap, 2018; Ratcliff et al., 2010). The variable is calculated by subtracting 
# the drift rate (i.e., 'the mean rate of information uptake' per individual; Pexman & Yap, 
# 2018, p. 5) based on the responses to abstract words from the drift rate based on responses 
# to concrete words.

PexmanYap2018_individual$information_uptake = 
  PexmanYap2018_individual$v_abs - PexmanYap2018_individual$v_con

# Add participants' information uptake and gender into the data set
semanticdecision = 
  merge(semanticdecision, 
        PexmanYap2018_individual[, c('Participant', 
                                     'information_uptake', 'Gender')], 
        all.x = TRUE, by = 'Participant')

str(semanticdecision)

# Free up workspace
# rm(PexmanYap2018_individual)

# Rename column
semanticdecision = semanticdecision %>% rename(participant_gender = Gender)

# Participant gender data
semanticdecision %>% 
  group_by(participant_gender) %>% 
  tally(n_distinct(Participant))

# ^ Result
# participant_gender     n
# <chr>              <int>
# F                    224
# M                     82
# NA                     6

# Recode dichotomous predictor 'participant_gender' (Brauer & Curtin, 2018; https://doi.org/10.1037/met0000159). 
# Male = -0.5, female = 0.5, others = 0 (N.B. the current data, shown above, does not contain sufficient 
# information to allow a more specific coding).

semanticdecision$recoded_participant_gender = 
  ifelse(semanticdecision$participant_gender == 'M', -0.5, 
         ifelse(semanticdecision$participant_gender == 'F', 0.5, 0))

# Replace NA values in recoded_participant_gender with zeros
semanticdecision[
  is.na(semanticdecision$recoded_participant_gender), 
  'recoded_participant_gender'] = 0

# View
summary(semanticdecision$recoded_participant_gender)
semanticdecision %>% 
  group_by(recoded_participant_gender) %>% 
  tally(n_distinct(Participant))


# DATA SET 3. Word-level computational measures from Wingfield and Connell (2022; https://psyarxiv.com/hpm4z/), 
# downloaded from https://osf.io/k4fn2/download?version=1

# This data set also contains lexical measures from the English Lexicon Project (Balota et al., 2007; https://doi.org/10.3758/BF03193014),
# namely, number of syllables, orthographic Levenshtein distance (Yarkoni et al., 2008; https://doi.org/10.3758/PBR.15.5.971),
# and phonological Levenshtein distance (Yap et al., 2009; https://doi.org/10.1016/j.jml.2009.02.001).
# The two latter measures were created by the authors cited, and added into the English Lexicon Project.

# Note on reproducibility
# The code below was used to download and save the data set in the folder 'semanticdecision/data/primary_datasets'. 
# To prevent any influence from future changes to the original data set online, the code below was protected (i.e., 
# commented out) by inserting the snippet `# Protected code # ` at the beginning of each line. If desired, the code
# can be run by removing the protective snippet from every line.

# Protected code # read.csv('https://osf.io/k4fn2/download?version=1') %>%
# Protected code #   write.csv('semanticdecision/data/primary_datasets/Wingfield_Connell_2019_semanticdecision.csv', 
# Protected code #             row.names = FALSE)

# Read in data set
Wingfield_Connell_2019_semanticdecision = 
  read.csv('semanticdecision/data/primary_datasets/Wingfield_Connell_2019_semanticdecision.csv')

# Descriptives
length(Wingfield_Connell_2019_semanticdecision)  # variables
nrow(Wingfield_Connell_2019_semanticdecision)    # rows
length(unique(Wingfield_Connell_2019_semanticdecision$Word))   # unique words
head(Wingfield_Connell_2019_semanticdecision[, 1:14], 10)    # Look at first 14 variables

# Wingfield and Connell (2022) found in their semantic decision study that certain corpus measures were the best predictors
# of task performance. Below, we're creating a difference score with two of these measures, namely, distance to the word 
# 'abstract' and distance to the word 'concrete'. The reason for making the difference score, rather than including both 
# separately, is the fact that the measures were each found to contribute independently by Wingfield and Connell, but they
# are very highly correlated (r = .99), as shown below.

cor(Wingfield_Connell_2019_semanticdecision$Conditional_probability_BNC_r5_correlation_abstract_distance,
    Wingfield_Connell_2019_semanticdecision$Conditional_probability_BNC_r5_correlation_concrete_distance, 
    use = 'complete.obs') %>% round(2)

# The difference score is created below by subtracting the correlation distance 
# to the word 'concrete' from the correlation distance to the word 'abstract'.

Wingfield_Connell_2019_semanticdecision$word_cooccurrence = 
  Wingfield_Connell_2019_semanticdecision$Conditional_probability_BNC_r5_correlation_abstract_distance -
  Wingfield_Connell_2019_semanticdecision$Conditional_probability_BNC_r5_correlation_concrete_distance

# Rename variables
Wingfield_Connell_2019_semanticdecision = 
  Wingfield_Connell_2019_semanticdecision %>%
  rename(word_frequency = elex_LgSUBTLWF,
         number_syllables = elex_NSyll,
         orthographic_Levenshtein_distance = elex_OLD,
         phonological_Levenshtein_distance = elex_PLD)

# Merge data sets
semanticdecision = 
  merge(semanticdecision, 
        Wingfield_Connell_2019_semanticdecision %>%
          select(c(Word, word_frequency, number_syllables, 
                   orthographic_Levenshtein_distance, 
                   phonological_Levenshtein_distance, 
                   Conditional_probability_BNC_r5_correlation_abstract_distance,
                   Conditional_probability_BNC_r5_correlation_concrete_distance,
                   word_cooccurrence)), 
        all.x = TRUE)

# Free up space
# rm(semanticdecision)


# DATA SET 4. Lancaster Sensorimotor Norms (Lynott et al., 2020; https://doi.org/10.3758/s13428-019-01316-z),
# downloaded from https://osf.io/48wsc/download?version=1

# Note on reproducibility
# The code below was used to download and save the data set in the folder 'general_datasets'. Such a folder was used
# because this data set was used in more than one study. To prevent any influence from future changes to the original 
# data set online, the code below was protected (i.e., commented out) by inserting the snippet `# Protected code # ` 
# at the beginning of each line. If desired, the code can be run by removing the protective snippet from every line.

# Protected code # read.csv('https://osf.io/48wsc/download?version=1') %>%
# Protected code #   write.csv('general_datasets/Lynott_etal_2020_LancasterSensorimotorNorms.csv', 
# Protected code #             row.names = FALSE)

# Read in data set
Lynott_etal_2020_LancasterSensorimotorNorms = 
  read.csv('general_datasets/Lynott_etal_2020_LancasterSensorimotorNorms.csv')

# Summary
str(Lynott_etal_2020_LancasterSensorimotorNorms)

# Rename columns
Lynott_etal_2020_LancasterSensorimotorNorms =
  Lynott_etal_2020_LancasterSensorimotorNorms %>%
  rename(visual_rating = Visual.mean)

# Make words lowercase to match semanticdecision data set.
Lynott_etal_2020_LancasterSensorimotorNorms$Word = 
  tolower(Lynott_etal_2020_LancasterSensorimotorNorms$Word)

# Number of words present in both the semanticdecision data set and the Lancaster data set.
length(intersect(semanticdecision$Word, 
                 Lynott_etal_2020_LancasterSensorimotorNorms$Word))

# Import Lancaster norms data
semanticdecision = 
  merge(semanticdecision, 
        Lynott_etal_2020_LancasterSensorimotorNorms[, c('Word', 'visual_rating')], 
        all.x = TRUE)

# Free up space
# rm(Lynott_etal_2020_LancasterSensorimotorNorms)


# DATA SET 5. Word concreteness (Brysbaert et al., 2014; https://doi.org/10.3758/s13428-013-0403-5), 
# downloaded from:
# https://static-content.springer.com/esm/art%3A10.3758%2Fs13428-013-0403-5/MediaObjects/13428_2013_403_MOESM1_ESM.xlsx

# Note on reproducibility
# The code below was used to download and save the data set in the folder 'general_datasets'. Such a folder was used
# because this data set was used in more than one study. To prevent any influence from future changes to the original 
# data set online, the code below was protected (i.e., commented out) by inserting the snippet `# Protected code # ` 
# at the beginning of each line. If desired, the code can be run by removing the protective snippet from every line.

# Protected code # GET('https://static-content.springer.com/esm/art%3A10.3758%2Fs13428-013-0403-5/MediaObjects/13428_2013_403_MOESM1_ESM.xlsx', 
# Protected code #     write_disk(tf <- tempfile(fileext = ".xlsx")))
# Protected code # read_excel(tf) %>%
# Protected code #   write.csv('general_datasets/Brysbaert_etal_2014_wordconcreteness.csv', 
# Protected code #             row.names = FALSE)

# Read in data set
Brysbaert_etal_2014_wordconcreteness = 
  read.csv('general_datasets/Brysbaert_etal_2014_wordconcreteness.csv')

# Rename column
Brysbaert_etal_2014_wordconcreteness =
  Brysbaert_etal_2014_wordconcreteness %>% rename(word_concreteness = Conc.M)

# Merge with general data set
semanticdecision = 
  merge(semanticdecision, 
        Brysbaert_etal_2014_wordconcreteness[, c('Word', 'word_concreteness')], 
        all.x = TRUE, by = 'Word')

# Number of words that have a word concreteness score
length(unique(semanticdecision[
  !is.na(semanticdecision$word_concreteness), 'Word']))

# Number of words lacking a word concreteness score
length(unique(semanticdecision[
  is.na(semanticdecision$word_concreteness), 'Word']))

# Free up workspace
# rm(Brysbaert_etal_2014_wordconcreteness)


# Consolidate general data set

# Remove any rows that have NAs in either the participant or the word fields
semanticdecision = 
  semanticdecision[complete.cases(semanticdecision$Participant) & 
                     complete.cases(semanticdecision$Word),]

# Calculate number of letters per word
semanticdecision$word_length = nchar(semanticdecision$Word)

# Final number of participants
length(unique(semanticdecision$Participant))

# Final number of words
length(unique(semanticdecision$Word))

# Number of words that don't have a word frequency measure
length(unique(semanticdecision[is.na(semanticdecision$word_frequency), 'Word']))

# Number of words that are lacking the number of syllables
length(unique(semanticdecision[is.na(semanticdecision$number_syllables), 'Word']))

# Number of words that don't have an orthographic Levenshtein distance measure
length(unique(semanticdecision[
  is.na(semanticdecision$orthographic_Levenshtein_distance), 'Word']))

# Number of words that don't have a phonological Levenshtein distance measure
length(unique(semanticdecision[
  is.na(semanticdecision$phonological_Levenshtein_distance), 'Word']))

# View common data set
str(semanticdecision)
head(semanticdecision)
nrow(semanticdecision)
length(unique(semanticdecision$Word))
nrow(semanticdecision[is.na(semanticdecision$Word),])    # No NA words

# Set variable classes
semanticdecision$Participant = as.factor(semanticdecision$Participant)
semanticdecision$participant_gender = as.factor(semanticdecision$participant_gender)
semanticdecision$vocabulary_size = as.numeric(semanticdecision$vocabulary_size)
semanticdecision$information_uptake = as.numeric(semanticdecision$information_uptake)
semanticdecision$Word = as.factor(semanticdecision$Word)
semanticdecision$TrialBlock = as.factor(semanticdecision$TrialBlock)
semanticdecision$RTclean = as.numeric(as.character(semanticdecision$RTclean))
semanticdecision$word_concreteness = 
  as.numeric(as.character(semanticdecision$word_concreteness))
semanticdecision$word_frequency = 
  as.numeric(as.character(semanticdecision$word_frequency))
semanticdecision$number_syllables = 
  as.numeric(as.character(semanticdecision$number_syllables))
semanticdecision$orthographic_Levenshtein_distance = 
  as.numeric(as.character(semanticdecision$orthographic_Levenshtein_distance))
semanticdecision$phonological_Levenshtein_distance = 
  as.numeric(as.character(semanticdecision$phonological_Levenshtein_distance))
semanticdecision$word_length = 
  as.numeric(as.character(semanticdecision$word_length))
semanticdecision$word_cooccurrence = 
  as.numeric(as.character(semanticdecision$word_cooccurrence))
semanticdecision$visual_rating = 
  as.numeric(as.character(semanticdecision$visual_rating))

str(semanticdecision)

# Definitions and names of variables
# z-scored word frequency: z_word_frequency
# z-scored word length: z_word_length
# z-scored word concreteness: z_word_concreteness
# z-scored vocabulary size: z_vocabulary_size
# z-scored word co-occurrence: z_word_cooccurrence
# z-scored visual modality rating: z_visual_rating


#######################################################################################################

# Remove NAs, necessary because the power analysis that is to be conducted using the 'simr' package 
# requires NA-free data (see https://github.com/pitakakariki/simr/issues/204).

semanticdecision = na.omit(semanticdecision)

str(semanticdecision)

# ! NOTE added after conducting this study: This removal of NA cases inadvertently 
# resulted in the removal of 6 participants who had NA in 'participant_gender'.

#######################################################################################################


# Trim RTs to 3 standard deviations within participants and within trial blocks, as done in the
# Calgary Semantic Decision study (Pexman et al., 2017; https://doi.org/10.3758/s13428-016-0720-6)

# Create empty dataframe using column names from the original data set.
new_semanticdecision = semanticdecision[0,]

# Loop over participants and trial blocks
for(i in unique(semanticdecision$Participant)) {
  for(j in unique(semanticdecision$TrialBlock)) {
    
    # First, select Participant and TrialBlock
    subset = semanticdecision[semanticdecision$Participant == i & 
                                semanticdecision$TrialBlock == j,]
    
    result = subset[subset$RTclean > -(mean(subset$RTclean) + 3 * sd(subset$RTclean)) &
                      subset$RTclean < mean(subset$RTclean) + 3 * sd(subset$RTclean),]
    
    new_semanticdecision = rbind(new_semanticdecision, result)
  }
}

# View percentage of trials trimmed
((nrow(semanticdecision) - nrow(new_semanticdecision)) / 
    nrow(semanticdecision)) * 100

# Apply result to final data set
semanticdecision = new_semanticdecision


# Z-score RT around each participant's own mean, following Faust et al. (1999;
# also see Pexman et al., 2017; Pexman & Yap, 2018; Yap et al., 2012, 2017).
semanticdecision$z_RTclean = scale_by(RTclean ~ Participant, semanticdecision)

# Z-score between-participants predictors, following Brauer and Curtin (2018; 
# https://doi.org/10.1037/met0000159)
semanticdecision$z_vocabulary_size = 
  scale(semanticdecision$vocabulary_size)
semanticdecision$z_information_uptake = 
  scale(semanticdecision$information_uptake)
semanticdecision$z_recoded_participant_gender = 
  scale(semanticdecision$recoded_participant_gender)

# Z-score between-items predictors around each participant's own mean, 
# following Brauer and Curtin (2018; https://doi.org/10.1037/met0000159)
semanticdecision$z_word_frequency = 
  scale_by(word_frequency ~ Participant, semanticdecision)
semanticdecision$z_word_length = 
  scale_by(word_length ~ Participant, semanticdecision)
semanticdecision$z_number_syllables = 
  scale_by(number_syllables ~ Participant, semanticdecision)
semanticdecision$z_orthographic_Levenshtein_distance = 
  scale_by(orthographic_Levenshtein_distance ~ Participant, semanticdecision)
semanticdecision$z_phonological_Levenshtein_distance = 
  scale_by(phonological_Levenshtein_distance ~ Participant, semanticdecision)
semanticdecision$z_word_concreteness = 
  scale_by(word_concreteness ~ Participant, semanticdecision)
semanticdecision$z_word_cooccurrence = 
  scale_by(word_cooccurrence ~ Participant, semanticdecision)
semanticdecision$z_visual_rating = 
  scale_by(visual_rating ~ Participant, semanticdecision)


# Select and order columns to be kept
semanticdecision = 
  semanticdecision %>%
  select(Participant, vocabulary_size, z_vocabulary_size, information_uptake, 
         z_information_uptake, participant_gender, recoded_participant_gender, 
         z_recoded_participant_gender, Word, TrialBlock, word_concreteness, 
         z_word_concreteness, word_length, z_word_length, word_frequency, 
         z_word_frequency, number_syllables, z_number_syllables, 
         orthographic_Levenshtein_distance, z_orthographic_Levenshtein_distance, 
         phonological_Levenshtein_distance, z_phonological_Levenshtein_distance, 
         Conditional_probability_BNC_r5_correlation_abstract_distance,
         Conditional_probability_BNC_r5_correlation_concrete_distance,
         word_cooccurrence, z_word_cooccurrence, visual_rating, z_visual_rating, 
         RTclean, z_RTclean)

# Save final data set
write.csv(semanticdecision, 
          'semanticdecision/data/final_dataset/semanticdecision.csv', 
          row.names = FALSE)

# Read back in
# semanticdecision = read.csv('semanticdecision/data/final_dataset/semanticdecision.csv')





# Part of Study 3: Lexical decision

# Preparation of final data set by merging several data sets from previous studies.

# All primary data sets were last downloaded on 3 January 2022.


# install.packages('curl')          # Data download
# install.packages('httr')          # Data download
# install.packages('readxl')        # Data reading
# install.packages('readr')         # Data reading
# install.packages('purrr')         # Data wrangling
# install.packages('dplyr')         # Data wrangling
# install.packages('stringr')       # Text processing
# install.packages('standardize')   # Used to cluster-mean center within-participants variables 

library(curl)
library(httr)
library(readxl)
library(readr)
library(purrr)
library(dplyr)
library(stringr)
library(standardize)


# DATA SET 1. English Lexicon Project (Balota et al., 2007; https://doi.org/10.3758/BF03193014)

# Download zip from https://osf.io/eu5ca/?version=1. There is one file for each of the 820 participants. 
# Inside note: The link below provides the first version of the file, which was used in the present 
# script in June 2021. Even though a second version was later added, in November 2021, to correct some 
# problems with the files (mentioned below in the current script), the first version is still used in 
# the current script, thanks to the version-specific link below.

# Note on reproducibility
# The code below was used to download and save the data set in the folder 'lexicaldecision/data/primary_datasets'. 
# Such a folder was used because this data set was used in more than one study. To prevent any influence from future 
# changes to the original data set online, the code below was protected (i.e., commented out) by inserting the snippet 
# `# Protected code # ` at the beginning of each line. If desired, the code can be run by removing the protective 
# snippet from every line.

# Protected code # curl_download('https://osf.io/eu5ca/download?version=1',
# Protected code #               destfile = 'lexicaldecision/data/primary_datasets/ldt.zip')

# Unzip
unzip('lexicaldecision/data/primary_datasets/ldt.zip', 
      exdir = 'lexicaldecision/data/primary_datasets/ldt_unzipped')

# List all files
files = list.files(path = 'lexicaldecision/data/primary_datasets/ldt_unzipped')

# Read in every file, saving each filename (`cbind` function 
# is used for the latter purpose)
all_files = Map(cbind, 
                lapply(paste('lexicaldecision/data/primary_datasets/ldt_unzipped', 
                             files, sep = '/'), read_csv,
                       # Arguments for read_csv
                       skip = 2, col_names = c('trial', 'word_number', 'realword', 
                                               'accuracy', 'RT', 'word'),
                       na = 'character()', col_types = 'cccccc'), 
                filename = files)

# Delete temporary files
unlink('lexicaldecision/data/primary_datasets/ldt_unzipped', 
       recursive = TRUE, force = TRUE)

# Apply several functions below to every participant's file
all_files_prep = lapply(all_files, function(x) {
  
  # First, extract participant information from the bottom of each csv file. These data appear as 
  # tables at the bottom of the files. The `lag` function selects the cell below the appropriate 
  # trait name. The `slice_tail` function selects the last occurrence of the information, 
  # necessary for a few files in which these data were entered twice, presumably due to errors 
  # in the first instance. Finally, these data are appended as new columns with the names 
  # specified at the beginning of the `select` function. In saving the  participants' IDS, the 
  # file name has to be used instead of the 'Subject' field because some of the IDs in the 
  # Subject field (ranging between 1 and 786) are repeated across the 820 files that are 
  # available at https://osf.io/eu5ca/?version=1.
  
  x = x %>%
    mutate(Participant = filename,
           dplyr::filter(x, dplyr::lag(realword == 'vocabAge')) %>% 
             select(vocabulary_age = realword) %>% slice_tail(),
           dplyr::filter(x, dplyr::lag(realword == 'vision')) %>% 
             select(participant_vision = realword) %>% slice_tail(),
           dplyr::filter(x, dplyr::lag(accuracy == 'hearing')) %>% 
             select(participant_hearing = accuracy) %>% slice_tail(),
           dplyr::filter(x, dplyr::lag(word_number == 'Gender')) %>% 
             select(participant_gender = word_number) %>% slice_tail() ) %>%
    
    # Second, remove other metadata from the files
    dplyr::filter(!is.na(as.numeric(trial)) & !is.na(as.numeric(word_number)) & 
                    !is.na(as.numeric(realword)) & !is.na(as.numeric(accuracy)) & 
                    !is.na(as.numeric(RT)) & complete.cases(word) );
  
  # Last, return the result of the above functions in each file.
  return(x)
})

# Consolidate the data from all participants into a single data set
lexicaldecision = all_files_prep %>% reduce(rbind) %>%
  
  # and order columns
  select(Participant, vocabulary_age, participant_vision, 
         participant_hearing, participant_gender, trial, 
         word, realword, RT, accuracy)

# Number of participants, words and rows by this point
lexicaldecision$Participant %>% unique %>% length   # 820
lexicaldecision$word %>% unique %>% length   # 80964
lexicaldecision %>% nrow    # 2767024

# Remove nonword trials, trials with an incorrect response, and trials with a response faster than 200 ms or 
# slower than 4,000 ms--i.e., trial time-out--(Balota et al., 2007; https://doi.org/10.3758/BF03193014).
new_lexicaldecision = 
  lexicaldecision[!lexicaldecision$realword == 0 & 
                    !lexicaldecision$accuracy == 0 & 
                    !lexicaldecision$RT < 200 & 
                    !lexicaldecision$RT > 4000,]

# Calculate number of participants removed in the trimming of outliers above,
# by subtracting the number of participants in the new data set from the 
# number in the original data set.
lexicaldecision$Participant %>% unique %>% length - 
  new_lexicaldecision$Participant %>% unique %>% length   # 10

# Calculate number of words removed in the trimming of outliers above
lexicaldecision$word %>% unique %>% length - 
  new_lexicaldecision$word %>% unique %>% length   # 58074

# Calculate number of rows removed in the trimming of outliers above
lexicaldecision %>% nrow - new_lexicaldecision %>% nrow   # 2730884

# Apply the trimming
lexicaldecision = new_lexicaldecision

# Number of words
length(unique(lexicaldecision$word))

# Calculate number of letters per word
lexicaldecision$word_length = nchar(lexicaldecision$word)

# Participants' gender data
lexicaldecision %>% 
  group_by(participant_gender) %>% tally(n_distinct(Participant))

# ^ Result
# participant_gender     n
# <chr>              <int>
# f                    539
# m                    262
# x                      9

# Recode dichotomous predictor 'participant_gender' (see Brauer & Curtin, 2018; https://doi.org/10.1037/met0000159).
# Male = -0.5, female = 0.5, others = 0 (N.B. the current data, shown above, does not contain sufficient 
# information to allow a more specific coding).

lexicaldecision$recoded_participant_gender = 
  ifelse(lexicaldecision$participant_gender == 'm', -0.5, 
         ifelse(lexicaldecision$participant_gender == 'f', 0.5, 0))

# View whether there is enough variability in the perceptual individual differences to 
# use these variables.

# Participants' vision data
lexicaldecision %>% group_by(participant_vision) %>% tally(n_distinct(Participant))

# ^ Result
# participant_vision     n
# <chr>              <int>
# -1                     1
# 0                     10
# 1                     13
# 2                     15
# 3                     43
# 4                     76
# 5                    157
# 6                    269
# 7                    226

# Participants' hearing data
lexicaldecision %>% 
  group_by(participant_hearing) %>% tally(n_distinct(Participant))

# ^ Result
# participant_hearing      n
# <chr>                <int>
# -1                       1
# 0                       10
# 1                        4
# 2                        9
# 3                       11
# 4                       37
# 5                      154
# 6                      320
# 7                      264

# Arguably, there is not enough variability for us to use these variables.


# DATA SET 2. Lexical measures from the English Lexicon Project (Balota et al., 2007; https://doi.org/10.3758/BF03193014),
# namely, number of syllables, orthographic Levenshtein distance (Yarkoni et al., 2008; https://doi.org/10.3758/PBR.15.5.971),
# and phonological Levenshtein distance (Yap et al., 2009; https://doi.org/10.1016/j.jml.2009.02.001).
# The two latter measures were created by the authors cited, and added into the English Lexicon Project.

# First, the target words were saved in R into a CSV file as follows:
write.csv(sort(unique(lexicaldecision$word)), 
          'lexicaldecision/data/primary_datasets/lexicaldecision_targetwords.csv', 
          row.names = FALSE)

# Next, the above file was uploaded to https://elexicon.wustl.edu/query14/query14.html, 
# where the 'Method of Submission' selected was 'Filename Containing List of Words'. 
# The default output variables 'Length', 'Log_Freq_HAL' and 'Log_Freq_HAL' were 
# deselected, whereas the output variables 'LgSUBTLWF', 'OLD', 'PLD' and 'NSyll' were 
# selected. The query was executed and the resulting table was copy-pasted into the 
# txt file that is loaded in below.

Balota_etal_2007_ELP_lexical = 
  read.csv('lexicaldecision/data/primary_datasets/Balota_etal_2007_ELP_lexical.csv')

# Rename columns
Balota_etal_2007_ELP_lexical =
  Balota_etal_2007_ELP_lexical %>%
  rename(word = Word, 
         word_frequency = LgSUBTLWF,
         orthographic_Levenshtein_distance = OLD,
         phonological_Levenshtein_distance = PLD,
         number_syllables = NSyll)

lexicaldecision = 
  merge(lexicaldecision, 
        Balota_etal_2007_ELP_lexical[, c('word', 'word_frequency', 
                                         'orthographic_Levenshtein_distance', 
                                         'phonological_Levenshtein_distance', 
                                         'number_syllables')], 
        by = 'word')

# Set variable classes
lexicaldecision$trial = as.numeric(lexicaldecision$trial)
lexicaldecision$vocabulary_age = as.numeric(lexicaldecision$vocabulary_age)
lexicaldecision$RT = as.numeric(lexicaldecision$RT)
lexicaldecision$word_frequency = as.numeric(lexicaldecision$word_frequency)
lexicaldecision$orthographic_Levenshtein_distance = 
  as.numeric(lexicaldecision$orthographic_Levenshtein_distance)
lexicaldecision$phonological_Levenshtein_distance = 
  as.numeric(lexicaldecision$phonological_Levenshtein_distance)
lexicaldecision$number_syllables = as.numeric(lexicaldecision$number_syllables)

# Remove empty rows
lexicaldecision = 
  lexicaldecision[complete.cases(lexicaldecision$Participant),]


# DATA SET 3. Lancaster Sensorimotor Norms (Lynott et al., 2020; https://doi.org/10.3758/s13428-019-01316-z),
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

# Adjust column names
Lynott_etal_2020_LancasterSensorimotorNorms =
  Lynott_etal_2020_LancasterSensorimotorNorms %>%
  rename(word = Word, visual_rating = Visual.mean)

# Make words lowercase to match lexicaldecision data set.
Lynott_etal_2020_LancasterSensorimotorNorms$word = 
  tolower(Lynott_etal_2020_LancasterSensorimotorNorms$word)

# Number of words present in both the lexicaldecision data set and the Lancaster data set.
length(intersect(lexicaldecision$word, 
                 Lynott_etal_2020_LancasterSensorimotorNorms$word))

# Import Lancaster norms data
lexicaldecision = 
  merge(lexicaldecision, 
        Lynott_etal_2020_LancasterSensorimotorNorms[, c('word', 'visual_rating')], 
        all.x = TRUE)

# Free up space
# rm(Lynott_etal_2020_LancasterSensorimotorNorms)


# DATA SET 4. Word concreteness (Brysbaert et al., 2014; https://doi.org/10.3758/s13428-013-0403-5), downloaded from:
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

# Rename columns
Brysbaert_etal_2014_wordconcreteness =
  Brysbaert_etal_2014_wordconcreteness %>%
  rename(word = Word, word_concreteness = Conc.M)

# Merge with general data set
lexicaldecision = 
  merge(lexicaldecision, 
        Brysbaert_etal_2014_wordconcreteness[, c('word', 'word_concreteness')], 
        all.x = TRUE, by = 'word', )

# Number of words that have a word concreteness score
length(unique(lexicaldecision[
  !is.na(lexicaldecision$word_concreteness), 'word']))

# Number of words lacking a word concreteness score
length(unique(lexicaldecision[
  is.na(lexicaldecision$word_concreteness), 'word']))

# Free up workspace
# rm(Brysbaert_etal_2014_wordconcreteness)


#######################################################################################################

# Remove NAs, necessary because the power analysis that is to be conducted using the 'simr' package 
# requires NA-free data (see https://github.com/pitakakariki/simr/issues/204).

lexicaldecision = na.omit(lexicaldecision)

str(lexicaldecision)

#######################################################################################################


# Number of participants and words now
lexicaldecision$Participant %>% unique %>% length   # 795
lexicaldecision$word %>% unique %>% length   # 12636


# Trim RTs to 3 standard deviations within participants, as done in the English 
# Lexicon Project (Balota et al., 2007; https://doi.org/10.3758/BF03193014).

# Create empty dataframe using column names from the original data set.
new_lexicaldecision = lexicaldecision[0,]

for(i in unique(lexicaldecision$Participant)) {
  
  # Select Participant and limit mean to 3 standard deviations
  result = lexicaldecision[lexicaldecision$Participant == i &
                             lexicaldecision$RT > -(mean(lexicaldecision$RT) + 3 * sd(lexicaldecision$RT)) &
                             lexicaldecision$RT < mean(lexicaldecision$RT) + 3 * sd(lexicaldecision$RT),]
  
  new_lexicaldecision = rbind(new_lexicaldecision, result)
}

# View percentage of trials trimmed
((nrow(lexicaldecision) - nrow(new_lexicaldecision)) / 
    nrow(lexicaldecision)) * 100
# 0


# Z-score RT around each participant's own mean, following Faust et al. (1999;
# also see Pexman et al., 2017; Pexman & Yap, 2018; Yap et al., 2012, 2017).
lexicaldecision$z_RT = scale_by(RT ~ Participant, lexicaldecision)

# Z-score between-participants predictors, following Brauer and Curtin 
# (2018; https://doi.org/10.1037/met0000159)
lexicaldecision$z_vocabulary_age = 
  scale(lexicaldecision$vocabulary_age)
lexicaldecision$z_recoded_participant_gender = 
  scale(lexicaldecision$recoded_participant_gender)

# Z-score between-items predictors around each participant's own mean, 
# following Brauer and Curtin (2018; https://doi.org/10.1037/met0000159)
lexicaldecision$z_word_frequency = 
  scale_by(word_frequency ~ Participant, lexicaldecision)
lexicaldecision$z_word_length = 
  scale_by(word_length ~ Participant, lexicaldecision)
lexicaldecision$z_number_syllables = 
  scale_by(number_syllables ~ Participant, lexicaldecision)
lexicaldecision$z_orthographic_Levenshtein_distance = 
  scale_by(orthographic_Levenshtein_distance ~ Participant, lexicaldecision)
lexicaldecision$z_phonological_Levenshtein_distance = 
  scale_by(phonological_Levenshtein_distance ~ Participant, lexicaldecision)
lexicaldecision$z_visual_rating = 
  scale_by(visual_rating ~ Participant, lexicaldecision)
lexicaldecision$z_word_concreteness = 
  scale_by(word_concreteness ~ Participant, lexicaldecision)


# Select and order columns to be kept
lexicaldecision = 
  lexicaldecision %>%
  select(Participant, vocabulary_age, z_vocabulary_age, participant_gender, 
         recoded_participant_gender, z_recoded_participant_gender, trial, 
         word, word_length, z_word_length, word_frequency, z_word_frequency, 
         number_syllables, z_number_syllables, 
         orthographic_Levenshtein_distance, z_orthographic_Levenshtein_distance, 
         phonological_Levenshtein_distance, z_phonological_Levenshtein_distance, 
         visual_rating, z_visual_rating, word_concreteness, z_word_concreteness, 
         RT, z_RT)

# Save final data set
write.csv(lexicaldecision, 
          'lexicaldecision/data/final_dataset/lexicaldecision.csv', 
          row.names = FALSE)

# Read back in
# lexicaldecision = read.csv('lexicaldecision/data/final_dataset/lexicaldecision.csv')


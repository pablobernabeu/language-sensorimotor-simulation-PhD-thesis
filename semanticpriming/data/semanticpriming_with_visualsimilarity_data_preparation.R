

# Part of Study 1: Semantic priming

# Preparation of data set WITH VISUAL SIMILARITY instead of visual-strength 
# difference. 

# Whereas visual-strength difference is based on Lynott et al.'s (2020) 
# sensorimotor norms, visual similarity was obtained from Petilli et al. 
# (2021). This data becomes considerably smaller than the original 
# 'semanticpriming' one because Petilli et al.'s visual similarity 
# scores are available only for around 25% of prime-target pairs.

# All primary data sets were last downloaded on 3 January 2022, with the 
# exception of Petilli et al.'s (2021) data set, which was downloaded on 
# 25 Apr 2022.


# install.packages('httr')          # Data download
# install.packages('readxl')        # Data reading
# install.packages('readr')         # Data reading
# install.packages('dplyr')         # Data wrangling
# install.packages('purrr')         # Data wrangling
# install.packages('Matrix')        # Data wrangling
# install.packages('LSAfun')        # Computation of cosine similarity (not using the LSA space but one described below)
# install.packages('standardize')   # Used to cluster-mean center within-participants variables 

library(httr)
library(readxl)
library(readr)
library(dplyr)
library(purrr)
library(Matrix)
library(LSAfun)
library(standardize)


# DATA SET 1. Lexical decision task from the Semantic Priming Project (Hutchison 
# et al., 2013; https://doi.org/10.3758/s13428-012-0304-z), downloaded from 
# https://www.montana.edu/attmemlab/documents/all%20ldt%20subs_all%20trials3.xlsx

# Note on reproducibility
# The code below was used to download and save the data set in the folder 'semanticpriming/data/primary_datasets'. 
# To prevent any influence from future changes to the original data set online, the code below was protected (i.e., 
# commented out) by inserting the snippet `# Protected code # ` at the beginning of each line. If desired, the code
# can be run by removing the protective snippet from every line.

# Protected code # GET('https://www.montana.edu/attmemlab/documents/all%20ldt%20subs_all%20trials3.xlsx', 
# Protected code #     write_disk(tf <- tempfile(fileext = ".xlsx")))
# Protected code # read_excel(tf) %>%
# Protected code #   write.csv('semanticpriming/data/primary_datasets/Hutchison_etal_2013_semanticpriming_lexicaldecision.csv', 
# Protected code #             row.names = FALSE)

# Read in data set
semanticpriming_with_visualsimilarity = 
  read.csv('semanticpriming/data/primary_datasets/Hutchison_etal_2013_semanticpriming_lexicaldecision.csv')

# Rename columns
semanticpriming_with_visualsimilarity = 
  semanticpriming_with_visualsimilarity %>%
  rename(Participant = Subject, target_word = target,
         prime_word = prime, interstimulus_interval = isi)

# Only keep correctly-responded trials
semanticpriming_with_visualsimilarity = 
  semanticpriming_with_visualsimilarity[semanticpriming_with_visualsimilarity$target.ACC == '1',]

# Remove responses faster than 200 ms or slower than 3,000 ms 
# (Hutchison et al., 2013; https://doi.org/10.3758/s13428-012-0304-z)
semanticpriming_with_visualsimilarity = 
  semanticpriming_with_visualsimilarity[!semanticpriming_with_visualsimilarity$target.RT < 200 & 
                                          !semanticpriming_with_visualsimilarity$target.RT > 3000,]

# Calculate number of letters per word
semanticpriming_with_visualsimilarity$target_length = 
  nchar(semanticpriming_with_visualsimilarity$target_word)

# Recode dichotomous predictor 'interstimulus_interval'
# (Brauer & Curtin, 2018; https://doi.org/10.1037/met0000159)
semanticpriming_with_visualsimilarity$recoded_interstimulus_interval = 
  ifelse(semanticpriming_with_visualsimilarity$interstimulus_interval == 50, -0.5, 
         ifelse(semanticpriming_with_visualsimilarity$interstimulus_interval == 1050, 0.5, 0))

# Process lingering NA values in recoded_interstimulus_interval
semanticpriming_with_visualsimilarity[
  is.na(semanticpriming_with_visualsimilarity$recoded_interstimulus_interval), 
  'recoded_interstimulus_interval'] = 0

# View
summary(semanticpriming_with_visualsimilarity$recoded_interstimulus_interval)
table(semanticpriming_with_visualsimilarity$recoded_interstimulus_interval)

# Keep first-associate and other-associate trials only, thus removing nonword trials (Yap et al., 2017; 
# https://www.montana.edu/attmemlab/documents/YHT_Individual_Differences_Priming.pdf). 

semanticpriming_with_visualsimilarity = 
  
  semanticpriming_with_visualsimilarity[semanticpriming_with_visualsimilarity$type == 'first' |
                                          semanticpriming_with_visualsimilarity$type == 'other', ] %>% 
  
  # Join prime and target words by a hyphen and save this into a new column, 
  # which will be used as a grouping factor in the mixed-effects model. This
  # is necessary because every target word was preceded by four prime words
  # over different trials. 
  mutate(primeword_targetword = paste0(prime_word, '_', target_word)) %>%
  
  # Select columns to keep
  select(Participant, prime_word, target_word, primeword_targetword, 
         Session, Block, Trial, interstimulus_interval, 
         recoded_interstimulus_interval, target_length, target.RT)

str(semanticpriming_with_visualsimilarity)
head(semanticpriming_with_visualsimilarity)


# DATA SET 2. Participants' characteristics, downloaded from the Semantic Priming Project:
# https://www.montana.edu/attmemlab/documents/LDT%20subject%20database.xlsx

# Note on reproducibility
# The code below was used to download and save the data set in the folder 'semanticpriming/data/primary_datasets'. 
# To prevent any influence from future changes to the original data set online, the code below was protected (i.e., 
# commented out) by inserting the snippet `# Protected code # ` at the beginning of each line. If desired, the code
# can be run by removing the protective snippet from every line.

# Protected code # GET('https://www.montana.edu/attmemlab/documents/LDT%20subject%20database.xlsx', 
# Protected code #     write_disk(tf <- tempfile(fileext = ".xlsx")))
# Protected code # read_excel(tf) %>%
# Protected code #   write.csv('semanticpriming/data/primary_datasets/Yap_etal_2017_individual.csv',
# Protected code #             row.names = FALSE)

# Read in data set
Yap_etal_2017_individual = 
  read.csv('semanticpriming/data/primary_datasets/Yap_etal_2017_individual.csv')

# Calculate single vocabulary score by averaging across the synonym, antonym and analogy tests 
# (as in Yap et al., 2017; https://www.montana.edu/khutchison/documents/YHT%20in%20press.pdf).
Yap_etal_2017_individual$vocabulary_size = 
  (Yap_etal_2017_individual$vocaba + 
     Yap_etal_2017_individual$vocabb + 
     Yap_etal_2017_individual$vocabc) / 3

# Rename columns
Yap_etal_2017_individual =
  Yap_etal_2017_individual %>%
  rename(Participant = SUBJECT, participant_gender = gender,
         attentional_control = ac)

# Merge general data set and subject characteristics
semanticpriming_with_visualsimilarity = 
  merge(semanticpriming_with_visualsimilarity, 
        Yap_etal_2017_individual[, c('Participant', 'participant_gender', 
                                     'vocabulary_size', 'attentional_control')], 
        by = 'Participant')

# Participant gender data
semanticpriming_with_visualsimilarity %>% group_by(participant_gender) %>% tally(n_distinct(Participant))

# ^ Result
# participant_gender     n
# <chr>              <int>
# f                    241
# F                     57
# m                    162
# M                     49
# wf                     1
# NA                     2

# Recode dichotomous predictor 'participant_gender' (Brauer & Curtin, 2018; https://doi.org/10.1037/met0000159). 
# Male = -0.5, female = 0.5, others = 0 (N.B. the current data, shown above, does not contain sufficient 
# information to allow a more specific coding).

semanticpriming_with_visualsimilarity$recoded_participant_gender = 
  ifelse(semanticpriming_with_visualsimilarity$participant_gender == 'M' | 
           semanticpriming_with_visualsimilarity$participant_gender == 'm', -0.5, 
         ifelse(semanticpriming_with_visualsimilarity$participant_gender == 'F' | 
                  semanticpriming_with_visualsimilarity$participant_gender == 'f' | 
                  semanticpriming_with_visualsimilarity$participant_gender == 'wf', 0.5, 0))

# Process lingering NA values in recoded_participant_gender
semanticpriming_with_visualsimilarity[is.na(semanticpriming_with_visualsimilarity$recoded_participant_gender), 
                                      'recoded_participant_gender'] = 0

# View
summary(semanticpriming_with_visualsimilarity$recoded_participant_gender)
semanticpriming_with_visualsimilarity %>% 
  group_by(recoded_participant_gender) %>% 
  tally(n_distinct(Participant))

# Free up workspace
# rm(Yap_etal_2017_individual)


# DATA SET 3. Lexical measures from the English Lexicon Project (Balota et al., 2007; https://doi.org/10.3758/BF03193014),
# namely, number of syllables, orthographic Levenshtein distance and phonological Levenshtein distance
# (Yarkoni et al., 2008; https://doi.org/10.3758/PBR.15.5.971).

# First, the target words were saved in R into a CSV file as follows:
write.csv(sort(unique(semanticpriming_with_visualsimilarity$target_word)), 
          'semanticpriming/data/primary_datasets/semanticpriming_targetwords.csv', 
          row.names = FALSE)

# Next, the above file was uploaded to https://elexicon.wustl.edu/query14/query14.html, 
# where the 'Method of Submission' selected was 'Filename Containing List of Words'. 
# The default output variables 'Length', 'Log_Freq_HAL' and 'Log_Freq_HAL' were 
# deselected, whereas the output variables 'LgSUBTLWF', 'OLD', 'PLD' and 'NSyll' were 
# selected. The query was executed and the resulting table was copy-pasted into the 
# txt file that is loaded in below.

Balota_etal_2007_ELP_lexical = 
  read.csv('semanticpriming/data/primary_datasets/Balota_etal_2007_ELP_lexical.csv')

# Rename columns
Balota_etal_2007_ELP_lexical =
  Balota_etal_2007_ELP_lexical %>%
  rename(target_word = Word, 
         target_word_frequency = LgSUBTLWF,
         target_orthographic_Levenshtein_distance = OLD,
         target_phonological_Levenshtein_distance = PLD,
         target_number_syllables = NSyll)

semanticpriming_with_visualsimilarity = 
  merge(semanticpriming_with_visualsimilarity, 
        Balota_etal_2007_ELP_lexical[, c('target_word', 'target_word_frequency', 
                                         'target_orthographic_Levenshtein_distance', 
                                         'target_phonological_Levenshtein_distance', 
                                         'target_number_syllables')], 
        by = 'target_word')

# Free up workspace
# rm(Balota_etal_2007_ELP_lexical)


# DATA SET 4. Prime-target linguistic cosine similarity

# The semantic space chosen
# Mandera et al. (2017; https://doi.org/10.1016/j.jml.2016.04.001) compared how different semantic spaces predicted 
# responses in the lexical decision task of the Semantic Priming Project (Hutchison et al., 2013). Table 5 in 
# Mandera et al. presents the explained variance achieved by each measure. The model using the most predictive 
# measure reached R^2 = .471. We tried to implement this measure (retrieved from 'english-lemmas-count-window.3-subtitle_en.zip' 
# at http://meshugga.ugent.be/snaut-downloads/spaces/english/count/), but it proved difficult because of the large 
# size of the semantic space. Specifically, the RAM required in R surpassed 90 Gigabytes, an amount of memory that 
# was difficult to reach even using a high-performance computing cluster, in which the job was queuing for several 
# days. The same difficulty when using the 'snaut' software instead of R (http://meshugga.ugent.be/snaut//download/). 
# Fortunately, as an alternative to the best semantic space, Table 5 in Mandera et al. (2017) shows that the 
# second-best semantic space in predicting the aforementioned responses achieved R^2 = .465. This semantic space 
# (retrieved from 'english-lemmas-cbow-window.6-dimensions.300-subtitle_en.w2v.gz' at 
# http://meshugga.ugent.be/snaut-downloads/spaces/english/predict/) was far smaller, allowing a feasible computation 
# even in a local machine. Therefore, the latter semantic space was downloaded below.

# Note on reproducibility
# The code below was used to download and save the data set in the folder 'semanticpriming/data/primary_datasets'. 
# To prevent any influence from future changes to the original data set online, the code below was protected (i.e., 
# commented out) by inserting the snippet `# Protected code # ` at the beginning of each line. If desired, the code
# can be run by removing the protective snippet from every line.

# Protected code # download.file('http://meshugga.ugent.be/snaut-downloads/spaces/english/predict/english-lemmas-cbow-window.6-dimensions.300-subtitle_en.w2v.gz',
# Protected code #               'semanticpriming/data/primary_datasets/Mandera_etal_2017_english-lemmas-cbow-window.6-dimensions.300-subtitle_en.w2v.gz')

# Read in data set as a matrix
Mandera_etal_2017_semanticspace = 
  data.matrix(read_delim(
    'semanticpriming/data/primary_datasets/Mandera_etal_2017_english-lemmas-cbow-window.6-dimensions.300-subtitle_en.w2v.gz',
    col_names = FALSE, delim = ' ',
    # Skip first seven lines, as they have metadata
    skip = 7) %>%
      # Remove first column as it has the row names, whilst this matrix 
      # must be fully numeric (row names to be added below)
      select(-1))

# Set words (first column) as row names
dimnames(Mandera_etal_2017_semanticspace)[1] = 
  read_delim(
    'semanticpriming/data/primary_datasets/Mandera_etal_2017_english-lemmas-cbow-window.6-dimensions.300-subtitle_en.w2v.gz',
    col_names = FALSE, delim = ' ',
    # Skip first seven lines, as they have metadata
    skip = 7) %>%
  # Row names containing the words
  select(1)

# Correct column names (X1 to X300)
colnames(Mandera_etal_2017_semanticspace) = paste0('X', rep(1:300))

# Obtain the unique combinations of prime and target words in the whole data set, discarding any repetitions
wordpairs = data.frame(
  unique(semanticpriming_with_visualsimilarity[, c('prime_word', 'target_word')]), 
  cosine_similarity = NA)

# Compute measure for each prime-target pair (prime word turned lowercase)
for(i in 1 : nrow(wordpairs)) {
  wordpairs[i, 'cosine_similarity'] = 
    LSAfun::Cosine(x = tolower(wordpairs[i, 'prime_word']), 
                   y = wordpairs[i, 'target_word'],
                   tvectors = Mandera_etal_2017_semanticspace)
}

# Add cosine_similarity to main data set.
semanticpriming_with_visualsimilarity = 
  merge(semanticpriming_with_visualsimilarity, wordpairs, 
        by = c('prime_word', 'target_word'), 
        all.x = TRUE)

# Free up workspace
# rm(Mandera_etal_2017_semanticspace)


# DATA SET 5. Lancaster Sensorimotor Norms (Lynott et al., 2020; https://doi.org/10.3758/s13428-019-01316-z),
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

# To match with Lancaster data set, make prime words lowercase

semanticpriming_with_visualsimilarity$prime_word = 
  tolower(semanticpriming_with_visualsimilarity$prime_word)

Lynott_etal_2020_LancasterSensorimotorNorms$Word = 
  tolower(Lynott_etal_2020_LancasterSensorimotorNorms$Word)

# Number of words present in both semanticpriming_with_visualsimilarity and
# in the Lancaster Sensorimotor Norms.

# Primes
length(intersect(semanticpriming_with_visualsimilarity$prime_word, 
                 Lynott_etal_2020_LancasterSensorimotorNorms$Word)) 

# Targets
length(intersect(semanticpriming_with_visualsimilarity$target_word, 
                 Lynott_etal_2020_LancasterSensorimotorNorms$Word))

# Import Lancaster norms data
# first for prime words
semanticpriming_with_visualsimilarity = 
  merge(semanticpriming_with_visualsimilarity, 
        Lynott_etal_2020_LancasterSensorimotorNorms %>%
          rename(prime_word = Word, 
                 prime_visual_rating = Visual.mean) %>% 
          select(prime_word, prime_visual_rating), 
        all.x = TRUE)

# now for target words
semanticpriming_with_visualsimilarity = 
  merge(semanticpriming_with_visualsimilarity, 
        Lynott_etal_2020_LancasterSensorimotorNorms %>%
          rename(target_word = Word, 
                 target_visual_rating = Visual.mean) %>% 
          select(target_word, target_visual_rating), 
        all.x = TRUE)

# Calculate prime-target difference
semanticpriming_with_visualsimilarity$visual_rating_diff = 
  semanticpriming_with_visualsimilarity$prime_visual_rating - 
  semanticpriming_with_visualsimilarity$target_visual_rating

# Free up workspace
# rm(Lynott_etal_2020_LancasterSensorimotorNorms)


# DATA SET 6. Distributional vision-based similarity (Petilli et al., 2021; 
# https://doi.org/10.1016/j.jml.2020.104194), downloaded from 
# https://osf.io/x4g7p/download?version=1

# Note on reproducibility
# The code below was used to download and save the data set in the folder 'general_datasets'. Such a folder was used
# because this data set was used in more than one study. To prevent any influence from future changes to the original 
# data set online, the code below was protected (i.e., commented out) by inserting the snippet `# Protected code # ` 
# at the beginning of each line. If desired, the code can be run by removing the protective snippet from every line.

# Protected code # read.csv('https://osf.io/x4g7p/download?version=1') %>%
# Protected code #   write.csv('semanticpriming/data/primary_datasets/Petilli_etal_2021_AggregatedDatabase.csv', 
# Protected code #             row.names = FALSE)

# Read in data set
Petilli_etal_2021_AggregatedDatabase = 
  read.csv('semanticpriming/data/primary_datasets/Petilli_etal_2021_AggregatedDatabase.csv')

# Summary
str(Petilli_etal_2021_AggregatedDatabase)

# This data set contains the prime and the target words in a single column 
# called 'Trial', both in upper case, and joined by a hyphen. Now, to 
# match the format of 'semanticpriming_with_visualsimilarity', make target 
# word (i.e., everything after the underscore) lowercase.

Petilli_etal_2021_AggregatedDatabase$Trial = 
  Petilli_etal_2021_AggregatedDatabase$Trial %>%
  gsub('(_.*)', '\\L\\1', ., perl = TRUE)

# Number of prime-target pairs present in both semanticpriming_with_visualsimilarity 
# and in Petilli et al.'s data set.
length(intersect(semanticpriming_with_visualsimilarity$primeword_targetword, 
                 Petilli_etal_2021_AggregatedDatabase$Trial)) 

# Import Petilli et al.'s vision-based similarity (i.e., 'VisualSimilarity_VGGF_L7') and
# name it 'visual_similarity'. The column 'Trial' is renamed as 'primeword_targetword'
# to allow the merging.

semanticpriming_with_visualsimilarity = 
  merge(semanticpriming_with_visualsimilarity, 
        Petilli_etal_2021_AggregatedDatabase %>%
          rename(primeword_targetword = Trial,
                 visual_similarity = VisualSimilarity_VGGF_L7) %>% 
          select(primeword_targetword, visual_similarity), 
        all.x = TRUE)

# Number and percentage of prime-target pairs that have a visual_similarity score.

n_distinct(semanticpriming_with_visualsimilarity[!is.na(semanticpriming_with_visualsimilarity$visual_similarity),
                                                 'primeword_targetword'])

# 1,098

(n_distinct(semanticpriming_with_visualsimilarity[!is.na(semanticpriming_with_visualsimilarity$visual_similarity),
                                                  'primeword_targetword']) /
    n_distinct(semanticpriming_with_visualsimilarity['primeword_targetword']) *
    100) %>% round(2)

# 17.17%

# Conversely, number and percentage of prime-target pairs that lack a visual_similarity score.

n_distinct(semanticpriming_with_visualsimilarity[is.na(semanticpriming_with_visualsimilarity$visual_similarity),
                                                 'primeword_targetword'])

# 5,296

(n_distinct(semanticpriming_with_visualsimilarity[is.na(semanticpriming_with_visualsimilarity$visual_similarity),
                                                  'primeword_targetword']) /
    n_distinct(semanticpriming_with_visualsimilarity['primeword_targetword']) *
    100) %>% round(2)

# 82.83%

# Free up workspace
# rm(Petilli_etal_2021_AggregatedDatabase)


# DATA SET 7. Word concreteness (Brysbaert et al., 2014; https://doi.org/10.3758/s13428-013-0403-5), downloaded from:
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

# Import concreteness data
# first for prime words
semanticpriming_with_visualsimilarity = 
  merge(semanticpriming_with_visualsimilarity, 
        Brysbaert_etal_2014_wordconcreteness %>%
          rename(prime_word = Word, 
                 prime_word_concreteness = Conc.M) %>% 
          select(prime_word, prime_word_concreteness), 
        all.x = TRUE)

# Revert the change of prime words into lowercase
semanticpriming_with_visualsimilarity$prime_word = 
  toupper(semanticpriming_with_visualsimilarity$prime_word)

# now for target words
semanticpriming_with_visualsimilarity = 
  merge(semanticpriming_with_visualsimilarity, 
        Brysbaert_etal_2014_wordconcreteness %>%
          rename(target_word = Word, 
                 target_word_concreteness = Conc.M) %>% 
          select(target_word, target_word_concreteness), 
        all.x = TRUE)

# Calculate prime-target difference
semanticpriming_with_visualsimilarity$word_concreteness_diff = 
  semanticpriming_with_visualsimilarity$prime_word_concreteness - 
  semanticpriming_with_visualsimilarity$target_word_concreteness

# Number of words that have a word-concreteness difference score
length(unique(semanticpriming_with_visualsimilarity[
  !is.na(semanticpriming_with_visualsimilarity$word_concreteness_diff), 'target_word']))

# Number of words lacking a word-concreteness difference score
length(unique(semanticpriming_with_visualsimilarity[
  is.na(semanticpriming_with_visualsimilarity$word_concreteness_diff), 'target_word']))

# Free up workspace
# rm(Brysbaert_etal_2014_wordconcreteness)


# Set variable classes
semanticpriming_with_visualsimilarity$cosine_similarity = 
  as.numeric(semanticpriming_with_visualsimilarity$cosine_similarity)
semanticpriming_with_visualsimilarity$target_word_frequency = 
  as.numeric(semanticpriming_with_visualsimilarity$target_word_frequency)
semanticpriming_with_visualsimilarity$target_orthographic_Levenshtein_distance = 
  as.numeric(semanticpriming_with_visualsimilarity$target_orthographic_Levenshtein_distance)
semanticpriming_with_visualsimilarity$target_phonological_Levenshtein_distance = 
  as.numeric(semanticpriming_with_visualsimilarity$target_phonological_Levenshtein_distance)


#######################################################################################################

# Remove NAs because the vast majority of prime-target pairs (around 75%) lack a visual similarity score.
# This proportion stems from the selection criteria applied by Petilli et al. (2021, 
# https://doi.org/10.1016/j.jml.2020.104194) while creating the variable.

semanticpriming_with_visualsimilarity = na.omit(semanticpriming_with_visualsimilarity)

str(semanticpriming_with_visualsimilarity)

#######################################################################################################


# Trim RTs to 3 standard deviations within participants, within sessions and within  
# interstimulus interval conditions, as done in the Semantic Priming Project (Hutchison 
# et al., 2013; https://doi.org/10.3758/s13428-012-0304-z).

# Create empty dataframe using column names from the original data set.
new_semanticpriming = semanticpriming_with_visualsimilarity[0,]

for(i in unique(semanticpriming_with_visualsimilarity$Participant)) {
  for(j in unique(semanticpriming_with_visualsimilarity$Session)) {
    for(k in unique(semanticpriming_with_visualsimilarity$interstimulus_interval)) {
      
      # First, select Participant, session and interstimulus interval condition
      subset = semanticpriming_with_visualsimilarity[semanticpriming_with_visualsimilarity$Participant == i & 
                                                       semanticpriming_with_visualsimilarity$Session == j & 
                                                       semanticpriming_with_visualsimilarity$interstimulus_interval == k,]
      
      result = subset[subset$target.RT > -(mean(subset$target.RT) + 3 * sd(subset$target.RT)) &
                        subset$target.RT < mean(subset$target.RT) + 3 * sd(subset$target.RT),]
      
      new_semanticpriming = rbind(new_semanticpriming, result)
    }
  }
}

# View percentage of trials trimmed
((nrow(semanticpriming_with_visualsimilarity) - nrow(new_semanticpriming)) / 
    nrow(semanticpriming_with_visualsimilarity)) * 100

# Apply change
semanticpriming_with_visualsimilarity = new_semanticpriming


# Z-score RT around each participant's own mean, following Faust et al. (1999;
# also see Pexman et al., 2017; Pexman & Yap, 2018; Yap et al., 2012, 2017).
semanticpriming_with_visualsimilarity$z_target.RT = 
  scale_by(target.RT ~ Participant, semanticpriming_with_visualsimilarity)

# Z-score between-participants predictors, following Brauer and Curtin (2018; 
# https://doi.org/10.1037/met0000159)
semanticpriming_with_visualsimilarity$z_vocabulary_size = 
  scale(semanticpriming_with_visualsimilarity$vocabulary_size)
semanticpriming_with_visualsimilarity$z_attentional_control = 
  scale(semanticpriming_with_visualsimilarity$attentional_control)
semanticpriming_with_visualsimilarity$z_recoded_participant_gender = 
  scale(semanticpriming_with_visualsimilarity$recoded_participant_gender)

# Z-score between-items predictors around each participant's own mean, 
# following Brauer and Curtin (2018; https://doi.org/10.1037/met0000159)
semanticpriming_with_visualsimilarity$z_target_word_frequency = 
  scale_by(target_word_frequency ~ Participant, semanticpriming_with_visualsimilarity)
semanticpriming_with_visualsimilarity$z_target_length = 
  scale_by(target_length ~ Participant, semanticpriming_with_visualsimilarity)
semanticpriming_with_visualsimilarity$z_target_number_syllables = 
  scale_by(target_number_syllables ~ Participant, semanticpriming_with_visualsimilarity)
semanticpriming_with_visualsimilarity$z_target_orthographic_Levenshtein_distance = 
  scale_by(target_orthographic_Levenshtein_distance ~ Participant, semanticpriming_with_visualsimilarity)
semanticpriming_with_visualsimilarity$z_target_phonological_Levenshtein_distance = 
  scale_by(target_phonological_Levenshtein_distance ~ Participant, semanticpriming_with_visualsimilarity)
semanticpriming_with_visualsimilarity$z_cosine_similarity = 
  scale_by(cosine_similarity ~ Participant, semanticpriming_with_visualsimilarity)
semanticpriming_with_visualsimilarity$z_visual_rating_diff = 
  scale_by(visual_rating_diff ~ Participant, semanticpriming_with_visualsimilarity)
semanticpriming_with_visualsimilarity$z_visual_similarity = 
  scale_by(visual_similarity ~ Participant, semanticpriming_with_visualsimilarity)
semanticpriming_with_visualsimilarity$z_word_concreteness_diff = 
  scale_by(word_concreteness_diff ~ Participant, semanticpriming_with_visualsimilarity)
semanticpriming_with_visualsimilarity$z_recoded_interstimulus_interval = 
  scale_by(recoded_interstimulus_interval ~ Participant, semanticpriming_with_visualsimilarity)


# Select and order columns to be kept
semanticpriming_with_visualsimilarity = 
  semanticpriming_with_visualsimilarity %>%
  select(Participant, vocabulary_size, z_vocabulary_size, attentional_control, 
         z_attentional_control, participant_gender, recoded_participant_gender, 
         z_recoded_participant_gender, prime_word, target_word, 
         primeword_targetword, Session, Block, Trial, interstimulus_interval, 
         recoded_interstimulus_interval, z_recoded_interstimulus_interval, 
         target_length, z_target_length, target_word_frequency, 
         z_target_word_frequency, target_number_syllables, 
         z_target_number_syllables, target_orthographic_Levenshtein_distance,
         z_target_orthographic_Levenshtein_distance, 
         target_phonological_Levenshtein_distance, 
         z_target_phonological_Levenshtein_distance, 
         cosine_similarity, z_cosine_similarity, visual_rating_diff, 
         z_visual_rating_diff, visual_similarity, z_visual_similarity, 
         word_concreteness_diff, z_word_concreteness_diff, 
         target.RT, z_target.RT)

# Save final data set
write.csv(semanticpriming_with_visualsimilarity, 
          'semanticpriming/data/subset_with_visualsimilarity/semanticpriming_with_visualsimilarity.csv', 
          row.names = FALSE)

# Read back in
# semanticpriming_with_visualsimilarity =
#   read.csv('semanticpriming/data/subset_with_visualsimilarity/semanticpriming_with_visualsimilarity.csv')


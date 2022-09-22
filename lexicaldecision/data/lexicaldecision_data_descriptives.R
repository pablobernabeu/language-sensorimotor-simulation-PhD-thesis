

# Part of Study 3: Lexical decision

# Data descriptives

library(dplyr)

# Read in data
lexicaldecision = read.csv('lexicaldecision/data/final_dataset/lexicaldecision.csv')

# Calculate number of words per participant

lexicaldecision_words_per_participant = 
  data.frame(participant = character(), number_of_words_per_participant = integer(), stringsAsFactors = FALSE)

for(i in unique(lexicaldecision[complete.cases(lexicaldecision$Participant) & 
                                complete.cases(lexicaldecision$word) & 
                                complete.cases(lexicaldecision$z_RT), 'Participant'])) {
  
  Participant = as.character(unique(lexicaldecision[lexicaldecision$Participant == i & 
                                                      complete.cases(lexicaldecision$z_RT), c('Participant')]))
  
  number_of_words_per_participant = 
    as.numeric(length(unique(lexicaldecision[lexicaldecision$Participant == i & 
                                               complete.cases(lexicaldecision$z_RT), c('word')])))
  
  result = data.frame(Participant, number_of_words_per_participant)
  
  lexicaldecision_words_per_participant = rbind(lexicaldecision_words_per_participant, result)
  
}

# Result per word
lexicaldecision_words_per_participant %>%
  arrange(number_of_words_per_participant)

# Summary
mean(lexicaldecision_words_per_participant$number_of_words_per_participant)
sd(lexicaldecision_words_per_participant$number_of_words_per_participant)
summary(lexicaldecision_words_per_participant$number_of_words_per_participant)


# Calculate number of participants per word

lexicaldecision_participants_per_word = 
  data.frame(word = character(), number_of_participants_per_word = integer(), stringsAsFactors = FALSE)

for(i in unique(lexicaldecision[complete.cases(lexicaldecision$word) & 
                                complete.cases(lexicaldecision$Participant) & 
                                complete.cases(lexicaldecision$z_RT), 'word'])) {
  
  word = as.character(unique(lexicaldecision[lexicaldecision$word == i & 
                                               complete.cases(lexicaldecision$z_RT), c('word')]))
  
  number_of_participants_per_word = 
    as.numeric(length(unique(lexicaldecision[lexicaldecision$word == i & 
                                               complete.cases(lexicaldecision$z_RT), c('Participant')])))
  
  result = data.frame(word, number_of_participants_per_word)
  
  lexicaldecision_participants_per_word = rbind(lexicaldecision_participants_per_word, result)
  
}

# Result per word
lexicaldecision_participants_per_word %>%
  arrange(number_of_participants_per_word)

# Summary
mean(lexicaldecision_participants_per_word$number_of_participants_per_word)
sd(lexicaldecision_participants_per_word$number_of_participants_per_word)
summary(lexicaldecision_participants_per_word$number_of_participants_per_word)


##############################################################################################################


# Visualise non-independence--that is, repeated measures--for different variables to better understand the
# appropriate random effects structure (see Brauer & Curtin, 2018; https://doi.org/10.1037/met0000159). 

library(ggplot2)

# Select a few participants and words only (i.e., the four with indices from 200 to 203)
lexicaldecision_3participants = 
  lexicaldecision[lexicaldecision$Participant %in% 
                    sort(unique(lexicaldecision$Participant))[200:203],]

lexicaldecision_3words = 
  lexicaldecision[lexicaldecision$word %in% 
                    sort(unique(lexicaldecision$word))[200:203],]

# Plots showing that vocabulary age varies between participants and within words, 
# thus requiring by-word random slopes.
ggplot(lexicaldecision_3participants, aes(Participant, vocabulary_age)) +
  geom_text(aes(label = word, color = word), 
            position = position_jitter(width = 0.3, height = 0)) +
  theme(legend.position = "none", panel.background = element_blank(), 
        strip.background = element_rect(colour = NA, fill = NA),
        panel.border = element_rect(fill = NA, color = "black")) +
  labs(y = 'Vocabulary age')

ggplot(lexicaldecision_3words, aes(word, vocabulary_age)) +
  geom_text(aes(label = Participant, color = Participant), 
            position = position_jitter(width = 0.3, height = 0)) +
  theme(legend.position = "none", panel.background = element_blank(), 
        strip.background = element_rect(colour = NA, fill = NA),
        panel.border = element_rect(fill = NA, color = "black")) +
  labs(y = 'Vocabulary age', x = 'Word')

# Plots showing that word frequency varies between words and within participants, 
# thus requiring by-participant random slopes.
ggplot(lexicaldecision_3participants, aes(Participant, word_frequency)) +
  geom_text(aes(label = word, color = word), 
            position = position_jitter(width = 0.3, height = 0)) +
  theme(legend.position = "none", panel.background = element_blank(), 
        strip.background = element_rect(colour = NA, fill = NA),
        panel.border = element_rect(fill = NA, color = "black")) +
  labs(y = 'Word frequency')

ggplot(lexicaldecision_3words, aes(word, word_frequency)) +
  geom_text(aes(label = Participant, color = Participant), 
            position = position_jitter(width = 0.3, height = 0)) +
  theme(legend.position = "none", panel.background = element_blank(), 
        strip.background = element_rect(colour = NA, fill = NA),
        panel.border = element_rect(fill = NA, color = "black")) +
  labs(y = 'Word frequency', x = 'Target word')



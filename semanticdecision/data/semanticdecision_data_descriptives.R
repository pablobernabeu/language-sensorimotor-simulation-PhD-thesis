

# Part of Study 2: Semantic decision

# Data descriptives

library(dplyr)

# Read in data
semanticdecision = read.csv('semanticdecision/data/final_dataset/semanticdecision.csv')

# Calculate number of words per participant

semanticdecision_words_per_participant = 
  data.frame(participant = character(), number_of_words_per_participant = integer(), stringsAsFactors = FALSE)

for(i in unique(semanticdecision[complete.cases(semanticdecision$Participant) & 
                                 complete.cases(semanticdecision$Word) & 
                                 complete.cases(semanticdecision$RTclean), 'Participant'])) {
  
  Participant = as.character(unique(semanticdecision[semanticdecision$Participant == i &
                                                       complete.cases(semanticdecision$RTclean), c('Participant')]))
  
  number_of_words_per_participant = 
    as.numeric(length(unique(semanticdecision[semanticdecision$Participant == i & 
                                                complete.cases(semanticdecision$RTclean), c('Word')])))
  
  result = data.frame(Participant, number_of_words_per_participant)
  
  semanticdecision_words_per_participant = rbind(semanticdecision_words_per_participant, result)
  
}

# Result per word
semanticdecision_words_per_participant %>%
  arrange(number_of_words_per_participant)

# Summary
mean(semanticdecision_words_per_participant$number_of_words_per_participant)
sd(semanticdecision_words_per_participant$number_of_words_per_participant)
summary(semanticdecision_words_per_participant$number_of_words_per_participant)


# Calculate number of participants per word

semanticdecision_participants_per_word = 
  data.frame(word = character(), number_of_participants_per_word = integer(), stringsAsFactors = FALSE)

for(i in unique(semanticdecision[complete.cases(semanticdecision$Word) & 
                                 complete.cases(semanticdecision$Participant) & 
                                 complete.cases(semanticdecision$RTclean), 'Word'])) {
  
  Word = as.character(unique(semanticdecision[semanticdecision$Word == i & 
                                                complete.cases(semanticdecision$RTclean), c('Word')]))
  
  number_of_participants_per_word = 
    as.numeric(length(unique(semanticdecision[semanticdecision$Word == i & 
                                                complete.cases(semanticdecision$RTclean), c('Participant')])))
  
  result = data.frame(Word, number_of_participants_per_word)
  
  semanticdecision_participants_per_word = rbind(semanticdecision_participants_per_word, result)
  
}

# Result per word
semanticdecision_participants_per_word %>%
  arrange(number_of_participants_per_word)

# Summary
mean(semanticdecision_participants_per_word$number_of_participants_per_word)
sd(semanticdecision_participants_per_word$number_of_participants_per_word)
summary(semanticdecision_participants_per_word$number_of_participants_per_word)


##############################################################################################################


# Visualise non-independence--that is, repeated measures--for different variables to better understand the
# appropriate random effects structure (see Brauer & Curtin, 2018; https://doi.org/10.1037/met0000159). 

library(ggplot2)

# Select a few participants and words only (i.e., the four with indices from 200 to 203)
semanticdecision_3participants = 
  semanticdecision[semanticdecision$Participant %in%
                     sort(unique(semanticdecision$Participant))[200:203],]

semanticdecision_3words = 
  semanticdecision[semanticdecision$Word %in%
                     sort(unique(semanticdecision$Word))[200:203],]

# Plots showing that vocabulary size varies between participants and within words, 
# thus requiring by-word random slopes.
ggplot(semanticdecision_3participants, aes(Participant, vocabulary_size)) +
  geom_text(aes(label = Word, color = Word), 
            position = position_jitter(width = 0.3, height = 0)) +
  theme(legend.position = "none", panel.background = element_blank(), 
        strip.background = element_rect(colour = NA, fill = NA),
        panel.border = element_rect(fill = NA, color = "black")) +
  labs(y = 'Vocabulary size')

ggplot(semanticdecision_3words, aes(Word, vocabulary_size)) +
  geom_text(aes(label = Participant, color = Participant), 
            position = position_jitter(width = 0.3, height = 0)) +
  theme(legend.position = "none", panel.background = element_blank(), 
        strip.background = element_rect(colour = NA, fill = NA),
        panel.border = element_rect(fill = NA, color = "black")) +
  labs(y = 'Vocabulary size', x = 'Word')

# Plots showing that word co-occurrence varies between words and within participants, 
# thus requiring by-participant random slopes.
ggplot(semanticdecision_3participants, aes(Participant, word_cooccurrence)) +
  geom_text(aes(label = Word, color = Word), 
            position = position_jitter(width = 0.3, height = 0)) +
  theme(legend.position = "none", panel.background = element_blank(), 
        strip.background = element_rect(colour = NA, fill = NA),
        panel.border = element_rect(fill = NA, color = "black")) +
  labs(y = 'Word co-occurrence')

ggplot(semanticdecision_3words, aes(Word, word_cooccurrence)) +
  geom_text(aes(label = Participant, color = Participant), 
            position = position_jitter(width = 0.3, height = 0)) +
  theme(legend.position = "none", panel.background = element_blank(), 
        strip.background = element_rect(colour = NA, fill = NA),
        panel.border = element_rect(fill = NA, color = "black")) +
  labs(y = 'Word co-occurrence', x = 'Word')





# Part of Study 2: Semantic priming

# Data descriptives

library(dplyr)

# Read in data
semanticpriming = read.csv('semanticpriming/data/final_dataset/semanticpriming.csv')

# Calculate number of words per participant

semanticpriming_words_per_participant = 
  data.frame(participant = character(), number_of_words_per_participant = integer(), stringsAsFactors = FALSE)

for(i in unique(semanticpriming[complete.cases(semanticpriming$Participant) & 
                                 complete.cases(semanticpriming$target_word) & 
                                 complete.cases(semanticpriming$target.RT), 'Participant'])) {
  
  Participant = as.character(unique(semanticpriming[semanticpriming$Participant == i &
                                                       complete.cases(semanticpriming$target.RT), c('Participant')]))
  
  number_of_words_per_participant = 
    as.numeric(length(unique(semanticpriming[semanticpriming$Participant == i & 
                                                complete.cases(semanticpriming$target.RT), c('target_word')])))
  
  result = data.frame(Participant, number_of_words_per_participant)
  
  semanticpriming_words_per_participant = rbind(semanticpriming_words_per_participant, result)
  
}

# Result per word
semanticpriming_words_per_participant %>%
  arrange(number_of_words_per_participant)

# Summary
mean(semanticpriming_words_per_participant$number_of_words_per_participant)
sd(semanticpriming_words_per_participant$number_of_words_per_participant)
summary(semanticpriming_words_per_participant$number_of_words_per_participant)


# Calculate number of participants per word

semanticpriming_participants_per_word = 
  data.frame(word = character(), number_of_participants_per_word = integer(), stringsAsFactors = FALSE)

for(i in unique(semanticpriming[complete.cases(semanticpriming$target_word) & 
                                 complete.cases(semanticpriming$Participant) & 
                                 complete.cases(semanticpriming$target.RT), 'target_word'])) {
  
  target_word = as.character(unique(semanticpriming[semanticpriming$target_word == i & 
                                                complete.cases(semanticpriming$target.RT), c('target_word')]))
  
  number_of_participants_per_word = 
    as.numeric(length(unique(semanticpriming[semanticpriming$target_word == i & 
                                                complete.cases(semanticpriming$target.RT), c('Participant')])))
  
  result = data.frame(target_word, number_of_participants_per_word)
  
  semanticpriming_participants_per_word = rbind(semanticpriming_participants_per_word, result)
  
}

# Result per word
semanticpriming_participants_per_word %>%
  arrange(number_of_participants_per_word)

# Summary
mean(semanticpriming_participants_per_word$number_of_participants_per_word)
sd(semanticpriming_participants_per_word$number_of_participants_per_word)
summary(semanticpriming_participants_per_word$number_of_participants_per_word)


##############################################################################################################


# Visualise non-independence--that is, repeated measures--for different variables to better understand the
# appropriate random effects structure (see Brauer & Curtin, 2018; https://doi.org/10.1037/met0000159). 

library(ggplot2)

# Select a few participants and words only (i.e., the four with indices from 200 to 203)
semanticpriming_3participants = 
  semanticpriming[semanticpriming$Participant %in%
                     sort(unique(semanticpriming$Participant))[200:203],]

semanticpriming_3words = 
  semanticpriming[semanticpriming$target_word %in%
                     sort(unique(semanticpriming$target_word))[200:203],]

# Plots showing that vocabulary size varies between participants and within words, 
# thus requiring by-word random slopes.
ggplot(semanticpriming_3participants, aes(Participant, vocabulary_size)) +
  geom_text(aes(label = target_word, color = target_word), 
            position = position_jitter(width = 0.3, height = 0)) +
  theme(legend.position = "none", panel.background = element_blank(), 
        strip.background = element_rect(colour = NA, fill = NA),
        panel.border = element_rect(fill = NA, color = "black")) +
  labs(y = 'Vocabulary size')

ggplot(semanticpriming_3words, aes(target_word, vocabulary_size)) +
  geom_text(aes(label = Participant, color = Participant), 
            position = position_jitter(width = 0.3, height = 0)) +
  theme(legend.position = "none", panel.background = element_blank(), 
        strip.background = element_rect(colour = NA, fill = NA),
        panel.border = element_rect(fill = NA, color = "black")) +
  labs(y = 'Vocabulary size', x = 'target_word')

# Plots showing that word co-occurrence varies between words and within participants, 
# thus requiring by-participant random slopes.
ggplot(semanticpriming_3participants, aes(Participant, word_cooccurrence)) +
  geom_text(aes(label = target_word, color = target_word), 
            position = position_jitter(width = 0.3, height = 0)) +
  theme(legend.position = "none", panel.background = element_blank(), 
        strip.background = element_rect(colour = NA, fill = NA),
        panel.border = element_rect(fill = NA, color = "black")) +
  labs(y = 'target_word co-occurrence')

ggplot(semanticpriming_3words, aes(target_word, word_cooccurrence)) +
  geom_text(aes(label = Participant, color = Participant), 
            position = position_jitter(width = 0.3, height = 0)) +
  theme(legend.position = "none", panel.background = element_blank(), 
        strip.background = element_rect(colour = NA, fill = NA),
        panel.border = element_rect(fill = NA, color = "black")) +
  labs(y = 'target_word co-occurrence', x = 'target_word')



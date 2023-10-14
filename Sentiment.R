# Loading packages
library(tidyverse)
library(tidytext)
library(textdata) 
library(stringr)
library(lubridate)
library(devtools)

# Load data
source("https://raw.githubusercontent.com/ClosestNeighbours/DS4I-Project-2/main/sona-first-steps%20(1).R")

# Load dictionaries
load(url("https://raw.githubusercontent.com/ClosestNeighbours/DS4I-Project-2/Sentiment-Analysis/dsfi-lexicons.Rdata"))

# Preprocess: tokenise & remove stop words
sona$president_13 <- as.factor(sona$president_13)
sona$year <- as.numeric(sona$year)
sona$date <- as.Date(sona$date,format = "%d-%m-%Y")

tidy_sona <- sona %>% unnest_tokens(word, speech, token = "words", to_lower = T) %>% 
  filter(!word %in% stop_words$word)

# Join bing lexicon
sona_sentiment <- tidy_sona %>% 
  left_join(bing, by = "word") %>%
  rename(bing_sentiment = sentiment) %>%
  mutate(bing_sentiment = ifelse(is.na(bing_sentiment), 'neutral', bing_sentiment)) %>% 
  mutate(bing_sentiment = ifelse(bing_sentiment == "oppressed", "negative", bing_sentiment)) %>%
  mutate(bing_sentiment = ifelse(bing_sentiment %in% c("honour", "hope"), "positive", bing_sentiment))         

# Join nrc lexicon
sona_sentiment <- sona_sentiment %>% 
  left_join(nrc, by = "word", relationship = "many-to-many") %>%
  rename(nrc_sentiment = sentiment) %>%
  mutate(nrc_sentiment = ifelse(is.na(nrc_sentiment), 'neutral', nrc_sentiment))

#### Top 20 positive words (bing) ####
sona_sentiment %>%
  filter(bing_sentiment == 'positive') %>%
  count(word) %>%
  arrange(desc(n)) %>%
  filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(reorder(word,n),n)) + 
  geom_col() + coord_flip() +
  labs(title = "Top 20 Positive Words", x = "Word", y = "Count")
  
#### Top 20 negative words (bing) ####
sona_sentiment %>%
  filter(bing_sentiment == 'negative') %>%
  count(word) %>%
  arrange(desc(n)) %>%
  filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(reorder(word,n),n)) + geom_col() + coord_flip() + 
  labs(title = "Top 20 Negative Words", x = "Word", y = "Count")

#### Sentiment by President while excluding neutral words (bing) ####
sentiment_prop <- sona_sentiment %>%
  group_by(president_13, bing_sentiment) %>%
  summarize(word_count = n()) %>% 
  filter(!bing_sentiment == "neutral")

ggplot(sentiment_prop, aes(x = president_13, y = word_count, fill = bing_sentiment)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Proportion of Positive to Negative Words by President",
    x = "President",
    y = "Word Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_discrete(name = "Sentiment", labels = c("Negative", "Positive"))

#### Top 10 positive words by President (bing) ####
top_pos_pres <- sona_sentiment %>%
  filter(bing_sentiment == "positive") %>%
  count(president_13, word) %>%
  group_by(president_13) %>%
  filter(rank(desc(n)) <= 10)

top_pos_pres %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_col(fill = "skyblue") +
  facet_wrap(~president_13, scales = "free") +
  coord_flip() +
  labs(title = "Top 10 Positive Words per President", x = "", y = "Frequency") 

#### Top 10 negative words by President (bing) ####
top_neg_pres <- sona_sentiment %>%
  filter(bing_sentiment == "negative") %>%
  count(president_13, word) %>%
  group_by(president_13) %>%
  filter(rank(desc(n)) <= 10)

top_neg_pres %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_col(fill = "skyblue") +
  facet_wrap(~president_13, scales = "free") +
  coord_flip() +
  labs(title = "Top 10 Negative Words per President", x = "", y = "Frequency") 

#### Sentiment barchart (nrc) ####
sona_sentiment %>%
  add_count(president_13, name = "n_words") %>%
  group_by(president_13, nrc_sentiment) %>%
  summarize(prop = n() / first(n_words)) %>% ungroup() %>%
  filter(nrc_sentiment != "neutral") %>%  
  ggplot(aes(nrc_sentiment, prop, fill = nrc_sentiment)) +
  geom_bar(stat = "identity") +
  facet_wrap(~president_13, scales = "free") +
  labs(title = "Sentiment Proportions by President", fill = "Sentiment", x = "Proportion", y = "Sentiment") +
  coord_flip() 

#### Sentiment over time ####
sentiments_time <- sona_sentiment %>%
  group_by(date, bing_sentiment) %>%
  summarize(n = n()) 

ggplot(filter(sentiments_time, bing_sentiment != 'neutral'), 
       aes(x = date, y = n, fill = bing_sentiment)) +
  geom_col() +
  labs(title = "Sentiments over Time", fill ="Sentiment", x = "Date", y = "Count")

sentiments_time <- sentiments_time %>% 
  left_join(sentiments_time %>% 
              group_by(date) %>% 
              summarise(total = sum(n))) %>%
  mutate(freq = n/total) 

sentiments_time %>% filter(bing_sentiment != 'neutral') %>%
  ggplot(aes(x = date, y = freq, colour = bing_sentiment)) +
  geom_line() + 
  geom_smooth(aes(colour = bing_sentiment)) +
  labs(title = "Sentiments over Time", fill ="Sentiment", x = "Date", y = "Frequency") 

#### Fit a linear model ####
model <- lm(freq ~ date, data = subset(sentiments_time, bing_sentiment == 'positive'))
summary(model)

#### Fit binomial GLM ####
glm_data <- sentiments_time %>%
  filter(bing_sentiment != "neutral")

glm_model <- glm(as.factor(bing_sentiment) ~ date, data = glm_data, family = "binomial")
summary(glm_model)

#### Aggregating sentiment over words ####
sentiments_per_speech <- sona_sentiment %>%
  group_by(filename) %>%
  summarize(net_sentiment = (sum(bing_sentiment == 'positive') - sum(bing_sentiment == 'negative')), date = first(date))

sentiments_per_speech %>%
  group_by(date) %>%
  summarize(prop_neg = sum(net_sentiment < 0) / n()) %>%
  ggplot(aes(x = date, y = prop_neg)) +
  geom_line() + geom_smooth()

#### Dealing with Negation ####
replace_reg <- '(http.*?(\\s|.$))|(www.*?(\\s|.$))|&amp;|&lt;|&gt;'

# Create bigrams and add sentiment for each word
bigrams_separated  <- sona %>%
  mutate(speech = str_replace_all(speech, replace_reg, '')) %>%
  unnest_tokens(bigram, speech, token = 'ngrams', n = 2) %>%
  separate(bigram, c('word1', 'word2'), sep = ' ')

bigrams_separated <- bigrams_separated %>% 
  left_join(bing, by = c(word1 = 'word')) %>%
  rename(sentiment1 = sentiment) %>%
  mutate(sentiment1 = ifelse(is.na(sentiment1), 'neutral', sentiment1)) %>%
  mutate(sentiment1 = ifelse(sentiment1 == "oppressed", "negative", sentiment1)) %>%
  mutate(sentiment1 = ifelse(sentiment1 %in% c("honour", "hope"), "positive", sentiment1)) %>%         

  left_join(bing, by = c(word2 = 'word')) %>%
  rename(sentiment2 = sentiment) %>%
  mutate(sentiment2 = ifelse(is.na(sentiment2), 'neutral', sentiment2)) %>%
  mutate(sentiment2 = ifelse(sentiment2 == "oppressed", "negative", sentiment2)) %>%
  mutate(sentiment2 = ifelse(sentiment2 %in% c("honour", "hope"), "positive", sentiment2)) %>%         
  
  select(date, word1, word2, sentiment1, sentiment2, everything())

# Reverse the sentiment if preceding word is negative
negation_words <- c('not', 'no', 'never', 'without', 'anti')

bigrams_separated <- bigrams_separated %>%
  
  # create a variable that is the opposite of sentiment2
  mutate(opp_sentiment2 = recode(sentiment2, 'positive' = 'negative',
                                 'negative' = 'positive',
                                 'neutral' = 'neutral')) %>%
  
  # reverse sentiment2 if word1 is a negation word
  mutate(sentiment2 = ifelse(word1 %in% negation_words, opp_sentiment2, sentiment2)) %>%
  
  # remove the opposite sentiment variable, which we don't need any more
  select(-opp_sentiment2)

# Calculate sentiment of each bigram
bigrams_separated <- bigrams_separated %>%
  mutate(net_sentiment = (sentiment1 == 'positive') + (sentiment2 == 'positive') - 
           (sentiment1 == 'negative') - (sentiment2 == 'negative')) %>%
  unite(bigram, word1, word2, sep = ' ', remove = FALSE)

# Plots
bigrams_separated %>%
  filter(net_sentiment > 0) %>% # get positive bigrams
  count(bigram, sort = TRUE) %>%
  filter(rank(desc(n)) < 20) %>%
  ggplot(aes(reorder(bigram,n),n)) + geom_col() + coord_flip() +
  labs(x = "", y="Count", title = "Top 20 Positive Bigrams")

bigrams_separated %>%
  filter(net_sentiment < 0) %>% # get negative bigrams
  count(bigram, sort = TRUE) %>%
  filter(rank(desc(n)) < 20) %>%
  ggplot(aes(reorder(bigram,n),n)) + geom_col() + coord_flip() +
  labs(x = "", y="Count", title = "Top 20 Negative Bigrams")

bigrams_separated %>%
  filter(net_sentiment < 0) %>% # get negative bigrams
  filter(word1 %in% negation_words) %>% # get bigrams where the first word is negation
  count(bigram, sort = TRUE) %>%
  arrange(desc(n)) %>% 
  head(20) %>%
  ggplot(aes(reorder(bigram, n), n)) + geom_col() + coord_flip() +
  labs(x = "", y = "Count", title = "Top 20 Negative Bigrams with Negation")

#### Playing around with Wordclouds ####
library(wordcloud)

tidy_sona %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

library(reshape2)

tidy_sona %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "blue"),
                   max.words = 100)

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
bing <- get_sentiments('bing') 
nrc <- get_sentiments('nrc') 
save(bing, nrc, file = "dsfi-lexicons.Rdata")

# Preprocess: tokenise & remove stop words
tidy_sona <- sona %>% unnest_tokens(word, speech, token = "words", to_lower = T) %>% 
  filter(!word %in% stop_words$word)

# Join bing lexicon
sona_sentiment <- tidy_sona %>% 
  left_join(bing, by = "word") %>%
  rename(bing_sentiment = sentiment) %>%
  mutate(bing_sentiment = ifelse(is.na(bing_sentiment), 'neutral', bing_sentiment))

# Top 20 positive words
sona_sentiment %>%
  filter(bing_sentiment == 'positive') %>%
  count(word) %>%
  arrange(desc(n)) %>%
  filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(reorder(word,n),n)) + geom_col() + coord_flip() + xlab('')  

# Top 20 negative words
sona_sentiment %>%
  filter(bing_sentiment == 'negative') %>%
  count(word) %>%
  arrange(desc(n)) %>%
  filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(reorder(word,n),n)) + geom_col() + coord_flip() + xlab('')  

# Sentiment by President (while excluding neutral words)
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

# Top 10 positive words by President
top_pos_pres <- sona_sentiment %>%
  filter(bing_sentiment == "positive") %>%
  count(president_13, word) %>%
  group_by(president_13) %>%
  filter(rank(desc(n)) <= 10)

top_pos_pres %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_col(fill = "skyblue") +
  facet_wrap(~president_13, scales = "free_y") +
  coord_flip() +
  xlab('') +
  labs(title = "Top 10 Positive Words per President", y = "Frequency") +
  theme_minimal() 

# Top 10 negative words by President
top_neg_pres <- sona_sentiment %>%
  filter(bing_sentiment == "negative") %>%
  count(president_13, word) %>%
  group_by(president_13) %>%
  filter(rank(desc(n)) <= 10)

top_neg_pres %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_col(fill = "skyblue") +
  facet_wrap(~president_13, scales = "free_y") +
  coord_flip() +
  xlab('') +
  labs(title = "Top 10 Negative Words per President", y = "Frequency") +
  theme_minimal() 



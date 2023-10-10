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
  mutate(bing_sentiment = ifelse(is.na(bing_sentiment), 'neutral', bing_sentiment))

# Join nrc lexicon
sona_sentiment <- sona_sentiment %>% 
  left_join(nrc, by = "word", relationship = "many-to-many") %>%
  rename(nrc_sentiment = sentiment) %>%
  mutate(nrc_sentiment = ifelse(is.na(nrc_sentiment), 'neutral', nrc_sentiment))

# Top 20 positive words (bing)
sona_sentiment %>%
  filter(bing_sentiment == 'positive') %>%
  count(word) %>%
  arrange(desc(n)) %>%
  filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(reorder(word,n),n)) + 
  geom_col() + coord_flip() +
  labs(title = "Top 20 Positive Words", x = "Word", y = "Count")
  

# Top 20 negative words (bing)
sona_sentiment %>%
  filter(bing_sentiment == 'negative') %>%
  count(word) %>%
  arrange(desc(n)) %>%
  filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(reorder(word,n),n)) + geom_col() + coord_flip() + 
  labs(title = "Top 20 Negative Words", x = "Word", y = "Count")

# Sentiment by President while excluding neutral words (bing)
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

# Top 10 positive words by President (bing)
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

# Top 10 negative words by President (bing)
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

# Sentiment chart (nrc)
sona_sentiment %>%
  add_count(president_13, name = "n_words") %>% 
  group_by(president_13, nrc_sentiment) %>%
  summarize(prop = n() / first(n_words)) %>% ungroup() %>%
  group_by(president_13, nrc_sentiment) %>%
  summarize(mean_prop = mean(prop)) %>% ungroup() %>%
  ggplot(aes(reorder(nrc_sentiment, mean_prop), mean_prop, fill = president_13)) + 
  geom_bar(stat = "identity", position = 'dodge') + coord_flip() +
  labs(title = "Proportion of words per Sentiment", fill ="President", x = "", y = 'Mean Proportion')

# Sentiment over time
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

# Fit a linear model
model <- lm(freq ~ date, data = subset(sentiments_time, bing_sentiment == 'positive'))
summary(model)

# Fit binomial GLM
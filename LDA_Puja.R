
library(stringr)
library(tidytext)
library(tidyverse)


#reading in data
source("https://raw.githubusercontent.com/ClosestNeighbours/DS4I-Project-2/LDA---Puja/editted_sona_first_steps.R")

#preprocessing
sona$date[36] <- "09-02-2023"
x <- sona$speech

y <- sub('^\\w*\\s*\\w*\\s*\\w*\\s*', '', x[1:34])
sona$speech[1:34] <- y

z <- sub("^[A-Za-z]+, \\d{1,2} [A-Za-z]+ \\d{4}  ", "", x[35])
sona$speech[35] <- z

a <- sub("\\d{1,2} [A-Za-z]+ \\d{4}", "", x[36])
sona$speech[36] <- a

sona$speech <- str_replace_all(sona$speech, "[^[:alnum:]]", " ")

sona$speech <- gsub('[[:digit:]]+', '', x)    #remove numbers


## tidy format
tidy_sona <- sona %>% 
  unnest_tokens(word, speech, token = 'words', to_lower = T) %>%
  filter(!word %in% stop_words$word)

## words per president
tidy_sona %>%
  group_by(president_13) %>% count(word) %>% summarise(n = n())
  
## common words
tidy_sona %>%
  count(word, sort = TRUE) %>% slice_head(n = 20)

# Identify the most common words for each president
most_common_words_per_pres <- tidy_sona %>% group_by(president_13) %>% count(word) %>% arrange(desc(n)) %>%
  slice_head(n = 20) %>%
  ungroup()

ggplot(most_common_words_per_pres, aes(x = reorder(word, n), y = n, fill = president_13)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 15 Words by President",
       x = "Word",
       y = "Frequency") +
  theme_minimal() + coord_flip() + theme(legend.position = "none") +
  facet_wrap(~ president_13, scales = "free")




exclude_words <- c("government", "people", "south")

most_common_words_filtered <- most_common_words_per_pres %>%
  filter(!word %in% exclude_words)

# Create the plot using the filtered data frame
ggplot(most_common_words_filtered, aes(x = reorder(word, n), y = n, fill = president_13)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 15 Words by President",
       x = "Word",
       y = "Frequency") +
  theme_minimal() + coord_flip() + theme(legend.position = "none") +
  facet_wrap(~ president_13, scales = "free")





## lda

library(topicmodels)

sona_tdf <- tidy_sona %>%
  group_by(president_13,word) %>%
  count() %>%  
  ungroup() 

dtm_sona <- sona_tdf %>% 
  cast_dtm(president_13, word, n)


sona_lda <- LDA(dtm_sona, k = 6, control = list(seed = 5291))
str(sona_lda)

sona_topics <- tidy(sona_lda, matrix = 'beta')
head(sona_topics)

#top 20 terms in each topic
sona_topics %>%
  group_by(topic) %>%
  slice_max(n = 20, order_by = beta) %>% ungroup() %>%
  arrange(topic, -beta) %>%
  ggplot(aes(reorder(term, beta), beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = 'free') + coord_flip()



#greatest diff in beta values - not sure if helpful
beta_spread <- sona_topics %>%
  mutate(topic = paste0('topic', topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>%
  filter(topic2 > .005 | topic5 > .005) %>%
  mutate(log_ratio = log2(topic6 / topic1))

beta_spread %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = 'Log2 ratio of beta in topic 2 / topic 1') +
  coord_flip()


#gamma
gamma <- tidy(sona_lda, matrix = 'gamma') 
head(gamma)

sona_gamma <- left_join(sona %>% mutate(president_13 = as.character(president_13)), 
                           gamma,
                           by = c("president_13" = "document"), 
                           relationship = "many-to-many")

#misatken to be about topic 
sona_gamma %>% 
  filter(president_13 == "deKlerk") %>%
  filter(topic == 1 & gamma > 0.005) 

pres_group_by_topic <- sona_gamma %>% group_by(topic) %>% group_by(topic, president_13) %>%
  summarise(president_appearances = n_distinct(filename))





#interesting plots or observations




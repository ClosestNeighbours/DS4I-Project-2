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

sona$speech <- gsub('[[:digit:]]+', '', sona$speech)    #remove numbers

# Add a decade column to the dataframe
sona$decade <- 10 * floor(as.numeric(sona$year) / 10)

# Split the dataframe by decade
list_of_decades <- split(sona, sona$decade)
list_of_decades$`1990`

#1990s

tidy_1990 <- list_of_decades$`1990` %>% 
  unnest_tokens(word, speech, token = 'words', to_lower = T) %>%
  filter(!word %in% stop_words$word)
often_words <- tidy_sona %>%
  count(word, sort = TRUE) %>% slice_head(n = 20)

tdf_1990 <- tidy_1990 %>%
  group_by(president_13,word) %>%
  count() %>%  
  ungroup() %>%
  filter(!word %in% often_words$word)

dtm_sona_1990 <- tdf_1990 %>% 
  cast_dtm(president_13, word, n)


lda_1990 <- LDA(dtm_sona_1990, k = 3, control = list(seed = 5291))
str(lda_1990)

topics_1990 <- tidy(lda_1990, matrix = 'beta')
head(topics_1990)

#top 20 terms in each topic
topics_1990 %>%
  group_by(topic) %>%
  slice_max(n = 20, order_by = beta) %>% ungroup() %>%
  arrange(topic, -beta) %>%
  ggplot(aes(reorder(term, beta), beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = 'free') + coord_flip()







## 2000

tidy_2000 <- list_of_decades$`2000` %>% 
  unnest_tokens(word, speech, token = 'words', to_lower = T) %>%
  filter(!word %in% stop_words$word)
often_words <- tidy_sona %>%
  count(word, sort = TRUE) %>% slice_head(n = 20)

tdf_2000 <- tidy_2000 %>%
  group_by(president_13,word) %>%
  count() %>%  
  ungroup() %>%
  filter(!word %in% often_words$word)

dtm_sona_2000<- tdf_2000 %>% 
  cast_dtm(president_13, word, n)


lda_2000 <- LDA(dtm_sona_2000, k = 3, control = list(seed = 5291))
str(lda_2000)

topics_2000 <- tidy(lda_2000, matrix = 'beta')
head(topics_2000)

#top 20 terms in each topic
topics_2000 %>%
  group_by(topic) %>%
  slice_max(n = 20, order_by = beta) %>% ungroup() %>%
  arrange(topic, -beta) %>%
  ggplot(aes(reorder(term, beta), beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = 'free') + coord_flip()



## 2010

tidy_2010 <- list_of_decades$`2010` %>% 
  unnest_tokens(word, speech, token = 'words', to_lower = T) %>%
  filter(!word %in% stop_words$word)
often_words <- tidy_sona %>%
  count(word, sort = TRUE) %>% slice_head(n = 20)

tdf_2010 <- tidy_2010 %>%
  group_by(president_13,word) %>%
  count() %>%  
  ungroup() %>%
  filter(!word %in% often_words$word)

dtm_sona_2010<- tdf_2010 %>% 
  cast_dtm(president_13, word, n)


lda_2010 <- LDA(dtm_sona_2010, k = 3, control = list(seed = 5291))
str(lda_2000)

topics_2010 <- tidy(lda_2010, matrix = 'beta')
head(topics_2010)

#top 20 terms in each topic
topics_2010 %>%
  group_by(topic) %>%
  slice_max(n = 20, order_by = beta) %>% ungroup() %>%
  arrange(topic, -beta) %>%
  ggplot(aes(reorder(term, beta), beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = 'free') + coord_flip()



## 2020

tidy_2020 <- list_of_decades$`2020` %>% 
  unnest_tokens(word, speech, token = 'words', to_lower = T) %>%
  filter(!word %in% stop_words$word)
often_words <- tidy_sona %>%
  count(word, sort = TRUE) %>% slice_head(n = 20)

tdf_2020 <- tidy_2020 %>%
  group_by(president_13,word) %>%
  count() %>%  
  ungroup() %>%
  filter(!word %in% often_words$word)

dtm_sona_2020<- tdf_2020 %>% 
  cast_dtm(president_13, word, n)


lda_2020 <- LDA(dtm_sona_2020, k = 3, control = list(seed = 5291))
str(lda_2000)

topics_2020 <- tidy(lda_2020, matrix = 'beta')
head(topics_2020)

#top 20 terms in each topic
topics_2020 %>%
  group_by(topic) %>%
  slice_max(n = 20, order_by = beta) %>% ungroup() %>%
  arrange(topic, -beta) %>%
  ggplot(aes(reorder(term, beta), beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = 'free') + coord_flip()

write_csv(sona, file='sona.csv')

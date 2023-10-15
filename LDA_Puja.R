
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

sona$speech <- gsub('[[:digit:]]+', '', sona$speech)    #remove numbers


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




exclude_words <- c("government", "people", "south", "africa", "african")

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

#eliminate words that occur 80-90 times?

#best k value
library(tm)

k_values <- c(2:20)

lda_models <- list()

for (k in k_values) {
  lda_models[[as.character(k)]] <- LDA(dtm_sona, k, control = list(seed = 5291))
}

coherence_values <- numeric(length(k_values))

for (i in 1:length(k_values)) {
  lda_model <- lda_models[[as.character(k_values[i])]]
  coherence_values[i] <- topicdoc::topic_coherence(lda_model, dtm_sona)
}

plot(k_values, coherence_values, type = "b", xlab = "Number of Topics (k)", ylab = "Topic Coherence")

optimal_k <- k_values[which.max(coherence_values)]




#with optimal k 
sona_lda <- LDA(dtm_sona, k = 4, control = list(seed = 5291))
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





#individual presidents 

#mandela

mandela_tidy <- tidy_sona %>% filter(president_13 == "Mandela")

mandela_tdf <- mandela_tidy %>%
  group_by(president_13,word) %>%
  count() %>%  
  ungroup() 

dtm_mandela <- mandela_tdf %>% 
  cast_dtm(president_13, word, n)


# k_values <- c(2:20)
# 
# lda_models_mandela <- list()
# 
# for (k in k_values) {
#   lda_models_mandela[[as.character(k)]] <- LDA(dtm_mandela, k, control = list(seed = 5291))
# }
# 
# coherence_values_mandela <- numeric(length(k_values))
# 
# for (i in 2:length(k_values)) {
#   lda_models_mandela <- lda_models_mandela[[as.character(k_values[i])]]
#   coherence_values_mandela[i] <- topicdoc::topic_coherence(lda_models_mandela, dtm_mandela)
# }
# 
# plot(k_values, coherence_values_mandela, type = "b", xlab = "Number of Topics (k)", ylab = "Topic Coherence")
# 
# optimal_k <- k_values[which.max(coherence_values_mandela)]

mandela_lda <- LDA(dtm_mandela, k = 2, control = list(seed = 5291))
str(mandela_lda)

mandela_topics <- tidy(mandela_lda, matrix = 'beta')
head(mandela_topics)

#top 20 terms in each topic
mandela_topics %>%
  group_by(topic) %>%
  slice_max(n = 20, order_by = beta) %>% ungroup() %>%
  arrange(topic, -beta) %>%
  ggplot(aes(reorder(term, beta), beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = 'free') + coord_flip()





#zuma

zuma_tidy <- tidy_sona %>% filter(president_13 == "Zuma")

zuma_tdf <- zuma_tidy %>%
  group_by(president_13,word) %>%
  count() %>%  
  ungroup() 

dtm_zuma <- zuma_tdf %>% 
  cast_dtm(president_13, word, n)


# k_values <- c(2:20)
# 
# lda_models_zuma <- list()
# 
# for (k in k_values) {
#   lda_models_zuma[[as.character(k)]] <- LDA(dtm_zuma, k, control = list(seed = 5291))
# }
# 
# coherence_values_zuma <- numeric(length(k_values))
# 
# for (i in 1:length(k_values)) {
#   lda_models_zuma <- lda_models_zuma[[as.character(k_values[i])]]
#   coherence_values_zuma[i] <- topicdoc::topic_coherence(lda_models_zuma, dtm_zuma)
# }
# 
# plot(k_values, coherence_values_zuma, type = "b", xlab = "Number of Topics (k)", ylab = "Topic Coherence")
# 
# optimal_k <- k_values[which.max(coherence_values_zuma)]

zuma_lda <- LDA(dtm_zuma, k = 2, control = list(seed = 5291))
str(zuma_lda)

zuma_topics <- tidy(zuma_lda, matrix = 'beta')
head(zuma_topics)

#top 20 terms in each topic
zuma_topics %>%
  group_by(topic) %>%
  slice_max(n = 20, order_by = beta) %>% ungroup() %>%
  arrange(topic, -beta) %>%
  ggplot(aes(reorder(term, beta), beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = 'free') + coord_flip()





#mbeki

mbeki_tidy <- tidy_sona %>% filter(president_13 == "Mbeki")

mbeki_tdf <- mbeki_tidy %>%
  group_by(president_13,word) %>%
  count() %>%  
  ungroup() 

dtm_mbeki <- mbeki_tdf %>% 
  cast_dtm(president_13, word, n)


# k_values <- c(2:20)
# 
# lda_models_mbeki <- list()
# 
# for (k in k_values) {
#   lda_models_mbeki[[as.character(k)]] <- LDA(dtm_mbeki, k, control = list(seed = 5291))
# }
# 
# coherence_values_mbeki <- numeric(length(k_values))
# 
# for (i in 1:length(k_values)) {
#   lda_models_mbeki <- lda_models_mbeki[[as.character(k_values[i])]]
#   coherence_values_mbeki[i] <- topicdoc::topic_coherence(lda_models_mbeki, dtm_mbeki)
# }
# 
# plot(k_values, coherence_values_mbeki, type = "b", xlab = "Number of Topics (k)", ylab = "Topic Coherence")
# 
# optimal_k <- k_values[which.max(coherence_values_mbeki)]

mbeki_lda <- LDA(dtm_mbeki, k = 2, control = list(seed = 5291))
str(mbeki_lda)

mbeki_topics <- tidy(mbeki_lda, matrix = 'beta')
head(mbeki_topics)

#top 20 terms in each topic
mbeki_topics %>%
  group_by(topic) %>%
  slice_max(n = 20, order_by = beta) %>% ungroup() %>%
  arrange(topic, -beta) %>%
  ggplot(aes(reorder(term, beta), beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = 'free') + coord_flip()







#ramaphosa

ramaphosa_tidy <- tidy_sona %>% filter(president_13 == "Ramaphosa")

ramaphosa_tdf <- ramaphosa_tidy %>%
  group_by(president_13,word) %>%
  count() %>%  
  ungroup() 

dtm_ramaphosa <- ramaphosa_tdf %>% 
  cast_dtm(president_13, word, n)

# 
# k_values <- c(2:20)
# 
# lda_models_ramaphosa <- list()
# 
# for (k in k_values) {
#   lda_models_ramaphosa[[as.character(k)]] <- LDA(dtm_ramaphosa, k, control = list(seed = 5291))
# }
# 
# coherence_values_ramaphosa <- numeric(length(k_values))
# 
# for (i in 1:length(k_values)) {
#   lda_models_ramaphosa <- lda_models_ramaphosa[[as.character(k_values[i])]]
#   coherence_values_ramaphosa[i] <- topicdoc::topic_coherence(lda_models_ramaphosa, dtm_ramaphosa)
# }
# 
# plot(k_values, coherence_values_ramaphosa, type = "b", xlab = "Number of Topics (k)", ylab = "Topic Coherence")
# 
# optimal_k <- k_values[which.max(coherence_values_ramaphosa)]

ramaphosa_lda <- LDA(dtm_ramaphosa, k = 2, control = list(seed = 5291))
str(ramaphosa_lda)

ramaphosa_topics <- tidy(ramaphosa_lda, matrix = 'beta')
head(ramaphosa_topics)

#top 20 terms in each topic
ramaphosa_topics %>%
  group_by(topic) %>%
  slice_max(n = 20, order_by = beta) %>% ungroup() %>%
  arrange(topic, -beta) %>%
  ggplot(aes(reorder(term, beta), beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = 'free') + coord_flip()


#deKlerk

deKlerk_tidy <- tidy_sona %>% filter(president_13 == "deKlerk")

deKlerk_tdf <- deKlerk_tidy %>%
  group_by(president_13,word) %>%
  count() %>%  
  ungroup() 

dtm_deKlerk <- deKlerk_tdf %>% 
  cast_dtm(president_13, word, n)


# k_values <- c(2:20)
# 
# lda_models_deKlerk <- list()
# 
# for (k in k_values) {
#   lda_models_deKlerk[[as.character(k)]] <- LDA(dtm_deKlerk, k, control = list(seed = 5291))
# }
# 
# coherence_values_deKlerk <- numeric(length(k_values))
# 
# for (i in 1:length(k_values)) {
#   lda_models_deKlerk <- lda_models_deKlerk[[as.character(k_values[i])]]
#   coherence_values_deKlerk[i] <- topicdoc::topic_coherence(lda_models_deKlerk, dtm_deKlerk)
# }
# 
# plot(k_values, coherence_values_deKlerk, type = "b", xlab = "Number of Topics (k)", ylab = "Topic Coherence")
# 
# optimal_k <- k_values[which.max(coherence_values_deKlerk)]

deKlerk_lda <- LDA(dtm_deKlerk, k = 2, control = list(seed = 5291))
str(deKlerk_lda)

deKlerk_topics <- tidy(deKlerk_lda, matrix = 'beta')
head(deKlerk_topics)

#top 20 terms in each topic
deKlerk_topics %>%
  group_by(topic) %>%
  slice_max(n = 20, order_by = beta) %>% ungroup() %>%
  arrange(topic, -beta) %>%
  ggplot(aes(reorder(term, beta), beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = 'free') + coord_flip()


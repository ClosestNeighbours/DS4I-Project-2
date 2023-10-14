
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
  slice_head(n = 15) %>%
  ungroup()

ggplot(most_common_words_per_pres, aes(x = reorder(word, n), y = n, fill = president_13)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Words by President",
       x = "Word",
       y = "Frequency") +
  theme_minimal() + coord_flip() + theme(legend.position = "none") +
  facet_wrap(~ president_13, scales = "free")








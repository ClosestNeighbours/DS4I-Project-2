library(tidytext)
library(dplyr)
library(ggplot2)
library(tidyr)
library(wordcloud2)
library(tm)
library(topicmodels)


presidents <- df %>% select(c(president_13))

pres <- table(presidents)

pres_df <- as.data.frame(pres) %>% arrange(desc(Freq))
head(pres_df)
plot(pres_df)


speeches_text <- df %>% select(c(speech))
tidy_speeches <- speeches_text %>% unnest_tokens(word,speech)
text_count <- tidy_speeches %>% anti_join(stop_words)
text_count_num <- text_count %>% count(word,sort= TRUE)

head(text_count_num)

text_count_cloud <- as.data.frame(text_count_num)

names(text_count_cloud) <- c("words", "frequency")
wordcloud2(text_count_cloud[1:50, ])

tokenized_data <- df %>%
  unnest_tokens(word, speech)

sentiments_data <- tokenized_data %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(year, president_13) %>%
  summarise(sentiment_score = mean(value, na.rm=TRUE))

sentiments_data

plot <- ggplot(sentiments_data, aes(x=year, y=sentiment_score, color=president_13, group=president_13)) +
  geom_line(size=1) +
  geom_point(size=3) +
  labs(title="Sentiment Trend Over the Years", x="Year", y="Sentiment Score") +
  theme_minimal() +
  scale_x_discrete(breaks = unique(sentiments_data$year))

# Print the plot
print(plot)


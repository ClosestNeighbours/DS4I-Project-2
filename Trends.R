# Load packages
library(tidytext)
library(dplyr)
library(ggplot2)
library(tidyr)
library(wordcloud2)
library(tm)
library(topicmodels)
library(magrittr)
library(qdap)
library(lubridate)

df <- sona
# Simple EDA
presidents <- df %>% select(c(president_13))
pres <- table(presidents)
pres_df <- as.data.frame(pres) %>% arrange(desc(Freq))
head(pres_df)


## Word Count 
speeches_text <- df %>% select(c(speech))
# Split a column into tokens
tidy_speeches <- speeches_text %>% unnest_tokens(word,speech)
# Remove stop words
text_count <- tidy_speeches %>% anti_join(stop_words)
text_count_num <- text_count %>% count(word,sort= TRUE)
head(text_count_num)
text_count_cloud <- as.data.frame(text_count_num)
names(text_count_cloud) <- c("words", "frequency")
wordcloud2(text_count_cloud[1:50, ])

# Tokenize the speeches
tokenized_data <- df %>%
  unnest_tokens(word, speech)

# Get count value stats with regards to the sentiment

## AFINN is a list of words rated for valence with an integer between minus five (negative) 
## and plus five (positive). 
sentiments_data <- tokenized_data %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(year, president_13) %>%
  summarise(sentiment_score = mean(value, na.rm=TRUE))

sentiments_data

# Average Sentiment score Throughout the years
plot <- ggplot(sentiments_data, aes(x=year, y=sentiment_score, color=president_13, group=president_13)) +
  geom_line(size=1) +
  geom_point(size=3) +
  labs(title="Sentiment Trend Over the Years", x="Year", y="Sentiment Score") +
  theme_minimal() +
  scale_x_discrete(breaks = unique(sentiments_data$year))

# Print the plot
print(plot)



# Bar plot showing the sentiment score from afinn
plot2 <- ggplot(sentiments_data, aes(x=year, y=sentiment_score, fill=president_13)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Sentiment Score per Year", x="Year", y="Sentiment Score") +
  theme_minimal()

plot2

plot3 <- ggplot(sentiments_data, aes(x=president_13, y=sentiment_score, fill=president_13)) +
  geom_boxplot() +
  labs(title="Distribution of Sentiment Scores per President", x="President", y="Sentiment Score") +
  theme_minimal()

print(plot3)


avg_sentiments <- sona %>%
  unnest_tokens(word, speech) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(year, president_13) %>%
  summarise(sentiment = sum(value)) %>%
  ungroup()

sentiments_afinn <- sona %>%
  unnest_tokens(word, speech) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(year, president_13) 

result <- sentiments_afinn %>%
  group_by(year, value) %>%
  summarise(count = n()) %>%
  arrange(year, desc(count))

result2 <- sentiments_afinn %>%
  group_by(president_13, value) %>%
  summarise(count = n())

print(result)
print(result2)

ggplot(result, aes(x=factor(value), y=count, fill=factor(value))) + 
  geom_bar(stat="identity") + 
  facet_wrap(~year, scales="free_y") + 
  labs(x="Sentiment Value", y="Count", title="Count of Sentiment Values by Year") +
  theme_minimal()

ggplot(result2, aes(x=factor(value), y=count, fill=factor(value))) + 
  geom_bar(stat="identity") + 
  facet_wrap(~president_13, scales="free_y") + 
  labs(x="Sentiment Value", y="Count", title="Count of Sentiment Values by President") +
  theme_minimal()

avg_sentiment <- result %>%
  group_by(year) %>%
  summarize(average_sentiment = sum(value * count) / sum(count))

# pre/post comparisons

prepostsona <- sona %>%
  filter(grepl("pre|post", filename))


sentiments_pre_post <- prepostsona %>%
  unnest_tokens(word, speech) %>%
  inner_join(get_sentiments("afinn"))



sentiments_pre_post <- sentiments_pre_post %>%
  mutate(election_time = case_when(
    grepl("post", filename) ~ "post",
    grepl("pre", filename) ~ "pre",
    TRUE ~ NA_character_
  ))

post_pre <- sentiments_pre_post %>% group_by(president_13,election_time) %>% summarise(avg_sent=mean(value))

post_pre <- post_pre %>% filter(!(president_13 %in% c("Motlanthe", "deKlerk")))

ggplot(post_pre, aes(x = president_13, y = avg_sent, fill = election_time)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme_minimal() + 
  labs(title = "Average Sentiment Pre and Post Elections", 
       x = "President", 
       y = "Average Sentiment", 
       fill = "Election Time")

## maybe has something to do with the impeachment





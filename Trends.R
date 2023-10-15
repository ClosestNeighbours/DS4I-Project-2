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
  unnest_tokens(word, speech) %>% rename(President = president_13)

# Get count value stats with regards to the sentiment

## AFINN is a list of words rated for valence with an integer between minus five (negative) 
## and plus five (positive). 
sentiments_data <- tokenized_data %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(year, President) %>%
  summarise(sentiment_score = mean(value, na.rm=TRUE))

sentiments_data

# Average Sentiment score Throughout the years
plot <- ggplot(sentiments_data, aes(x=year, y=sentiment_score, color=President, group=President)) +
  geom_line(size=1) +
  geom_point(size=3) +
  labs(title="Sentiment Trend Over the Years", x="Year", y="Sentiment Score") +
  theme_minimal() +
  scale_x_discrete(breaks = unique(sentiments_data$year))

# Print the plot
print(plot)


# Bar plot showing the sentiment score from afinn
plot2 <- ggplot(sentiments_data, aes(x=year, y=sentiment_score, fill=President)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Sentiment Score per Year", x="Year", y="Sentiment Score") +
  theme_minimal()

plot2

plot3 <- ggplot(sentiments_data, aes(x=President, y=sentiment_score, fill=President)) +
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
  labs(x="Sentiment Value", y="Number of Occurances", title="Count of Sentiment Values by Year") +
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

post_pre$election_time <- factor(post_pre$election_time, levels = c("pre", "post"))

ggplot(post_pre, aes(x = president_13, y = avg_sent, fill = election_time)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme_minimal() + 
  labs(title = "Average Sentiment During Pre and Post Elections", 
       x = "Presidents", 
       y = "Average Sentiment", 
       fill = "Election Time")

## Decade Trends

decade <- tokenized_data %>%
  inner_join(get_sentiments("afinn")) 

decade$date <- dmy(decade$date)

decade <- decade %>%
  mutate(year = as.numeric(year),
         decade = 10 * (year %/% 10))

avg_sentiment_per_decade <- decade %>%
  group_by(decade) %>%
  summarise(avg_sentiment = mean(value, na.rm = TRUE))

print(avg_sentiment_per_decade)

ggplot(avg_sentiment_per_decade, aes(x = as.factor(decade), y = avg_sentiment)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Average Sentiment Per Decade (1990-2020)",
    x = "Decade",
    y = "Average Sentiment Value"
  ) + theme_minimal()

# List of keywords to search for
keywords <- c("loadshedding", "coronavirus", "electricity", "theft", "corruption", "rape", "apartheid", "racism")

# Convert speeches to lowercase for case-insensitive search
df$speech <- tolower(df$speech)

# Count occurrences for each president and keyword
count_data <- df %>%
  group_by(president_13) %>%
  summarise(across(starts_with("speech"), list(
    loadshedding = ~sum(str_count(., "loadshedding")),
    coronavirus = ~sum(str_count(., "coronavirus")),
    electricity = ~sum(str_count(., "electricity")),
    theft = ~sum(str_count(., "theft")),
    corruption = ~sum(str_count(., "corruption")),
    rape = ~sum(str_count(., "rape")),
    apartheid = ~sum(str_count(., "apartheid")),
    racism = ~sum(str_count(., "racism"))
  ))) %>%
  pivot_longer(-president_13, names_to="keyword", values_to="count")

count_data$keyword <- gsub("speech_", "", count_data$keyword)

count_data$president_13 <- factor(count_data$president_13, levels = c("deKlerk","Mandela", "Mbeki", "Motlanthe", "Zuma", "Ramaphosa"))

# Plot
ggplot(count_data, aes(x=president_13, y=count, fill=keyword)) +
  geom_bar(stat="identity", position="dodge") +
  theme_minimal() +
  labs(title="Topic Occurrences by President over the years", x="President", y="Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


for (keyword in keywords) {
  df_temp <- sona %>%
    group_by(year) %>%
    summarise(count = sum(str_count(speech, keyword))) %>%
    mutate(keyword = keyword)
  
  results <- bind_rows(sona, df_temp)
}

results <- results %>%
  arrange(year, keyword)

min_year <- as.numeric(min(results$year, na.rm = TRUE))
max_year <- as.numeric(max(results$year, na.rm = TRUE))

results <- results %>%
  mutate(year = as.numeric(year)) %>%
  mutate(year_bin = cut(year, breaks = seq(min_year, max_year+5, by = 5), labels = FALSE, include.lowest = TRUE)) %>%
  group_by(year_bin, keyword) %>%
  summarise(count = sum(count)) %>%
  mutate(bin_label = paste((year_bin-1)*5 + min_year, year_bin*5 + min_year - 1, sep = "-"))

results <- results %>% filter(!is.na(year_bin))

# Plot the data
ggplot(results, aes(x = bin_label, y = count, fill = keyword)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Keyword Occurrences in 5-Year Bins", x = "Year Bins", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))









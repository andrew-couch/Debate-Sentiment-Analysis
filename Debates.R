library(tidyverse)
library(tidytext)
library(sentimentr)
library(ggthemes)

df <- read.csv("Debate4.csv", stringsAsFactors = FALSE)
df$character <- as.factor(df$character)

df %>% 
  unnest_tokens(word, "text") %>% 
  filter(word == "applause") %>% 
  group_by(character) %>% 
  count(word) %>% 
  ggplot(aes(x = reorder(character, -n), 
             y = n, 
             color = character, 
             fill = character)) + 
  geom_point(size = 5) + 
  geom_segment(aes(x = character, 
                   y = n,
                   xend = character, 
                   yend = 0)) + 
  theme_economist() +
  ggtitle("Count of applauses")

df %>% 
  unnest_tokens(word, "text") %>% 
  anti_join(stop_words) %>% 
  count(character, word) %>% 
  bind_tf_idf(word, character, n) %>% 
  group_by(character) %>% 
  top_n(tf_idf, n = 5) %>% 
  ggplot(mapping = aes(x = reorder_within(word, tf_idf, character), 
             y = tf_idf, 
             color = character, 
             fill = character)) + 
  geom_col() + 
  scale_x_reordered() + 
  facet_wrap(~character, scales = "free") + 
  coord_flip() +
  theme_economist() + 
  ggtitle("Top tf-idf words")

df %>% 
  unnest_tokens(word, "text") %>% 
  anti_join(stop_words) %>% 
  count(character, word) %>% 
  group_by(character) %>% 
  top_n(n, n = 5) %>% 
  ggplot(mapping = aes(x = reorder_within(word, n, character), 
                       y = n, 
                       color = character, 
                       fill = character)) + 
  geom_col() + 
  scale_x_reordered() + 
  facet_wrap(~character, scales = "free") + 
  coord_flip() +
  theme_economist() + 
  ggtitle("Top 5 Bow")

df %>% 
  filter(character != "(UNKNOWN)") %>% 
  unnest_tokens(word, "text") %>% 
  anti_join(stop_words) %>% 
  filter(!word == "trump") %>% 
  inner_join( get_sentiments(lexicon = "bing")) %>% 
  group_by(character, sentiment) %>% 
  count(word,sentiment) %>% 
  spread(key = sentiment, value = n, fill = 0) %>% 
  gather(-character, -word, key = sentiment, value = n) %>% 
  group_by(character, sentiment) %>% 
  top_n(n, n = 5) %>% 
  mutate(n = if_else(sentiment == "positive", n, -n)) %>% 
  ggplot(aes(x = reorder_within(word, n, character), 
             y = n, 
             color = sentiment, 
             fill = sentiment)) + 
  geom_col() + 
  scale_x_reordered() + 
  facet_wrap(~character, scales = "free") + 
  coord_flip() + 
  ggtitle("Top 5 Positive Negative Words") +
  theme_economist()

df %>% 
  filter(character != "(UNKNOWN)") %>% 
unnest_tokens(bigram,"text",token = "ngrams", n = 2) %>% 
  group_by(character) %>% 
  count(character, bigram) %>% 
  bind_tf_idf(bigram, character,n) %>% 
  top_n(tf_idf, n = 5) %>% 
  ggplot(aes(x = reorder_within(bigram, tf_idf, character),
             y = tf_idf, 
             color = character, 
             fill = character)) + 
  geom_col() + 
  scale_x_reordered() + 
  facet_wrap(~character, scales = "free") + 
  coord_flip() + 
  theme_economist() + 
  ggtitle("Top 5 Bigrams by Tf-idf")

df %>% 
  filter(character != "(UNKNOWN)") %>% 
  get_sentences() %>% 
  sentiment(lexicon::hash_sentiment_jockers_rinker) %>% 
  filter(str_detect(`text`, "Trump")) %>% 
  ungroup() %>% 
  group_by(character) %>% 
  summarise(sentiment = mean(sentiment)) %>% 
  mutate(posneg = if_else(sentiment > 0, "Positive","Negative"))  %>% 
  ggplot(aes(x = reorder(character, sentiment), 
             y = sentiment, 
             color = posneg, 
             fill = posneg)) + 
  geom_col() +
  coord_flip() + 
  ggtitle("Average Sentence Sentiment when Trump is mentioned") +
  theme_economist()

df %>% 
  filter(character != "(UNKNOWN)") %>% 
  get_sentences() %>% 
  sentiment(lexicon::hash_sentiment_jockers_rinker) %>% 
  filter(str_detect(`text`, "Trump")) %>% 
  mutate(type = if_else(sentiment > 0, "Positive","Negative")) %>% 
  ggplot(aes(x = sentiment, fill = type)) + 
  geom_histogram(color = "white") + 
  ggtitle("Trump Mentions Sentiment Distribution") + 
  theme_economist()
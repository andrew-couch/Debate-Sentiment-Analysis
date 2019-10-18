library(tidyverse)
library(tidytext)
library(sentimentr)
library(ggthemes)
library(topicmodels)

df <- read.csv("DemDebates.csv")
df$text <- as.character(df$text)

df <- df %>% get_sentences()
df <- df %>% 
  filter(!character %in% c("(Unknown)", "Announcer","Bash","Bridgewater","Burnett","Cooper","Davis","Diaz-Balart","Guthrie","Holt","Jose Diaz-Balart","Lacey","Lemon","Lester Holt", "Maddow","Muir","Protesters","Protestor","Ramos","Savannah Guthrie","Stephanopoulos","Tapper","Todd","Unknown"))

df %>% 
  unnest_tokens(word, "text") %>% 
  anti_join(stop_words) %>% 
  count(character, word) %>% 
  bind_tf_idf(word, character, n) %>% 
  group_by(character) %>% 
  top_n(tf_idf, n = 5) %>% 
  ggplot(aes(x = reorder_within(word, tf_idf, character), 
             y = tf_idf, 
             color = character, 
             fill = character)) + 
  geom_col() + 
  scale_x_reordered() + 
  facet_wrap(~character, scales = "free") + 
  coord_flip()

df %>% 
  unnest_tokens(word, "text") %>% 
  anti_join(stop_words) %>% 
  filter(word != "trump") %>% 
  inner_join(get_sentiments(lexicon = "bing")) %>% 
  group_by(character, word, sentiment) %>% 
  count(sentiment) %>% 
  group_by(character, sentiment) %>% 
  top_n(n, n =5) %>% 
  mutate(n = if_else(sentiment == "positive",n,-n)) %>% 
  ggplot(aes(x = reorder_within(word, n, character), 
             y = n, 
             color = sentiment, 
             fill = sentiment)) + 
  geom_col() +
  scale_x_reordered() +
  facet_wrap(~character, scales = "free") + 
  coord_flip() + 
  theme_economist() + 
  ggtitle("Top 5 Positive/Negative Words using Bing Lexicon")



df %>% 
  unnest_tokens(bigram, "text", token = "ngrams", n = 2) %>% 
  count(character, bigram) %>% 
  bind_tf_idf(bigram ,character, n) %>% 
  group_by(character) %>% 
  top_n(tf_idf, n = 5) %>% 
  ggplot(aes(x = reorder_within(bigram, tf_idf, character), y = tf_idf, color = character, fill = character)) + 
  geom_col() + 
  scale_x_reordered() +
  facet_wrap(~character, scales = "free") + 
  coord_flip()

df %>% 
  unnest_tokens(trigram, "text", token = "ngrams", n = 3) %>% 
  count(character, trigram) %>% 
  group_by(character) %>% 
  top_n(n, n =5)

df %>% 
  unnest_tokens(trigram, "text", token = "ngrams", n = 3) %>% 
  count(character, trigram) %>% 
  bind_tf_idf(trigram, character,n) %>% 
  top_n(tf_idf, n =5)


#Debate words
df %>% 
  unnest_tokens(word, "text") %>% 
  anti_join(stop_words) %>% 
  count(debate, word) %>% 
  group_by(debate) %>% 
  top_n(n, n =5)

df %>% 
  unnest_tokens(word, "text") %>% 
  anti_join(stop_words) %>% 
  count(debate, word) %>% 
  bind_tf_idf(word, debate, n) %>% 
  group_by(debate) %>% 
  top_n(tf_idf, n = 5)


#Tableau Data

#Candidate Top words 
df %>% 
  unnest_tokens(word, "text") %>% 
  anti_join(stop_words) %>% 
  count(character, word) %>% 
  bind_tf_idf(word, character, n) %>% 
  group_by(character) %>% 
  top_n(n, n = 100)

df %>% 
  unnest_tokens(word, "text") %>% 
  anti_join(stop_words) %>% 
  count(character, word) %>% 
  group_by(character) %>% 
  top_n(n, n = 25)

df %>% 
  unnest_tokens(bigram, "text", token = "ngrams", n =2) %>% 
  count(character, bigram) %>% 
  bind_tf_idf(bigram, character, n) %>% 
  group_by(character) %>% 
  top_n(tf_idf, n = 25)

df %>% 
  unnest_tokens(trigram, "text", token = "ngrams", n =3) %>% 
  count(character, trigram) %>% 
  bind_tf_idf(trigram, character, n) %>% 
  group_by(character) %>% 
  top_n(tf_idf, n = 25)


#Debate Top words
df %>% 
  unnest_tokens(word, "text") %>% 
  anti_join(stop_words) %>% 
  count(debate, word) %>% 
  bind_tf_idf(word, debate, n) %>% 
  group_by(debate) %>% 
  top_n(n, n = 100) %>% 
  arrange(debate, -tf_idf)

df %>% 
  unnest_tokens(word, "text") %>% 
  anti_join(stop_words) %>% 
  count(debate, word)%>% 
  group_by(debate) %>% 
  top_n(n, n = 25) %>% 
  arrange(debate, -n)

df %>% 
  unnest_tokens(bigram, "text", token = "ngrams", n =2) %>% 
  count(debate, bigram) %>% 
  bind_tf_idf(bigram, debate, n) %>% 
  group_by(debate) %>% 
  top_n(tf_idf, n = 25) %>% 
  arrange(debate, -tf_idf)

df %>% 
  unnest_tokens(trigram, "text", token = "ngrams", n =3) %>% 
  count(debate, trigram) %>% 
  bind_tf_idf(trigram, debate, n) %>% 
  group_by(debate) %>% 
  top_n(tf_idf, n = 25) %>% 
  arrange(debate, -tf_idf)


#Topic Modeling
df %>% 
  unnest_tokens(word, "text") %>% 
  anti_join(stop_words) %>% 
  count(debate, word) %>% 
  ungroup() %>% 
  cast_dtm(debate, word, n) %>% 
  LDA(k = 5) %>% 
  tidy(matrix = "beta") %>% 
  group_by(topic) %>% 
  top_n(beta, n = 15) %>% 
  arrange(topic, -beta) %>% 
  ungroup() %>% 
  distinct(term, .keep_all = TRUE) %>% 
  ggplot(aes(x = reorder_within(term, beta, topic), 
             y = beta, 
             color = as.factor(topic), 
             fill = as.factor(topic))) + 
  geom_col() + 
  scale_x_reordered() + 
  facet_wrap(~topic, scales = "free") + 
  coord_flip() + 
  theme_economist()
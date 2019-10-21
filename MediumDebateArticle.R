library(tidyverse)
library(tidytext)

df <- read.csv("DemDebates.csv", colClasses = c("factor","factor","character"))
df <- df %>% 
  filter(!character %in% c("(Unknown)", "Announcer","Bash","Bridgewater","Burnett","Cooper","Davis","Diaz-Balart","Guthrie","Holt","Jose Diaz-Balart","Lacey","Lemon","Lester Holt", "Maddow","Muir","Protesters","Protestor","Ramos","Savannah Guthrie","Stephanopoulos","Tapper","Todd","Unknown"))

options(scipen = 999)


df %>% 
  unnest_tokens(word, "text", token = "ngrams", n =2) %>% 
  separate(word, c("word1","word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word | !word2 %in% stop_words$word) %>% 
  unite("bigram", c(word1, word2), sep = " ") %>% 
  count(character,bigram) %>% 
  group_by(character) %>% 
  top_n(n, n = 5) %>% 
  arrange(character, -n) %>% 
  ggplot(aes(x = reorder_within(bigram, n, character),#Reorders the words by freq
             y  = n,
             fill = character)) + 
  geom_col() + 
  scale_x_reordered() + #Reorders the words 
  facet_wrap(~character, scales ="free") + #Creates individual graphs
  coord_flip() + 
  theme(legend.position = "None")


df %>% 
  unnest_tokens(word, "text", token = "ngrams", n =1) %>% 
  count(character, word) %>% 
  bind_tf_idf(word, character, n) %>% 
  group_by(character) %>% 
  top_n(tf_idf, n = 5) %>% 
  ggplot(aes(x = reorder_within(word, tf_idf, character), y = tf_idf, fill = character)) + 
  geom_col() + 
  scale_x_reordered() + 
  facet_wrap(~character, scales = "free") + 
  coord_flip() + 
  theme(legend.position = "none")

bind_tf_idf()

df %>% 
  unnest_tokens(word, "text", token = "ngrams", n =2) %>% 
  anti_join(stop_words) %>% 
  count(character, word) %>% 
  group_by(character) %>% 
  top_n(n, n = 5) %>% 
  arrange(character, -n) %>% 
  ggplot(aes(x = reorder_within(word, n, character),#Reorders the words by freq
             y  = n,
             fill = character)) + 
  geom_col() + 
  scale_x_reordered() + #Reorders the words 
  facet_wrap(~character, scales ="free") + #Creates individual graphs
  coord_flip() + 
  theme(legend.position = "None")
  
df %>% 
  unnest_tokens(trigram, "text", token = "ngrams", n = 3) %>% 
  count(character, trigram) %>% 
  bind_tf_idf(trigram, character,n) %>% 
  group_by(character) %>% 
  top_n(tf_idf, n = 5) %>% 
  mutate(rank = rank(tf_idf, ties.method = "random")) %>% 
  arrange(character, rank) %>% 
  filter(rank <=5) %>% 
  ggplot(aes(x = reorder_within(trigram, tf_idf, character), 
             y = tf_idf, 
             color = character, 
             fill = character)) + 
  geom_col() +
  scale_x_reordered() +
  facet_wrap(~character, scales = "free") +
  coord_flip() + 
  theme(legend.position = "None")



ggsave("debate.png", plot = last_plot(), height = 9*1.5, width = 16*1.5, units = "in", dpi = 400)
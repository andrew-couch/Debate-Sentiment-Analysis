library(tidyverse)
library(tidytext)

df <- read.csv("DemDebates.csv", colClasses = c("factor","factor","character"))
df <- df %>% 
  filter(!character %in% c("(Unknown)", "Announcer","Bash","Bridgewater","Burnett","Cooper","Davis","Diaz-Balart","Guthrie","Holt","Jose Diaz-Balart","Lacey","Lemon","Lester Holt", "Maddow","Muir","Protesters","Protestor","Ramos","Savannah Guthrie","Stephanopoulos","Tapper","Todd","Unknown"))

options(scipen = 999)


df %>% unnest_tokens(word, "text", token = "ngrams", n =1)


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
  theme_fivethirtyeight() + 
  theme(legend.position = "None")



ggsave("debate.png", plot = last_plot(), height = 9*1.5, width = 16*1.5, units = "in", dpi = 400)
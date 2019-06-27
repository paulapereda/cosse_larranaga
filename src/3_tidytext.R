library(tidyverse)
library(tidytext)
library(tm)


vector <- file.path('.', 'data', 'stopes.csv') %>% 
  read.csv(sep = ";", 
           encoding = "latin1",
           stringsAsFactors = FALSE) %>% 
  transmute(stop = as.character(X........HEAD))

stop <- pull(vector, stop)

actividades_1 <- planilla %>%
  group_by(sexo_duenio) %>%
  filter(!is.na(p1a)) %>% 
  unnest_tokens(word, p1a) %>%
  count(word, sort = TRUE) %>%
  ungroup() %>%
  filter(!word %in% stop) %>%
  add_count(sexo_duenio) %>% 
  rename(total = nn) %>%
  bind_tf_idf(word, sexo_duenio, n) %>%
  select(-total) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(sexo_duenio) %>% 
  top_n(15)

conteo <- tweets_candidatos_df %>%
  group_by(candidato) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  count(word) %>%
  ungroup() %>% 
  filter(!word %in% stop) %>%
  filter(!str_detect(word, "@\\w+ *")) %>% 
  filter(!str_detect(word, "#\\w+ *")) %>% 
  filter(!(str_length(word) == 1)) %>% 
  filter(!(str_length(word) == 2)) %>% 
  filter(word != "larrañaga") %>% 
  filter(word != "recuperándose") %>% 
  filter(word != "internado") %>% 
  filter(word != "hermano") %>% 
  filter(word != "gustaría") %>% 
  filter(word != "decirle") %>%
  filter(word != "desco") %>% 
  filter(word != "jaguel") %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(candidato) %>% 
  top_n(20) %>% 
  arrange(candidato, n)

ggplot(conteo, aes(reorder(word, n), n)) +
   geom_bar(position = "dodge", stat = "identity") +
   facet_wrap(~ candidato, ncol = 2, scales = "free") +
   coord_flip() +
   theme_bw() +
   theme(text = element_text(family = "Georgia", color = "grey20")) +
   xlab('') +
   ylab('') + 
  ggsave('imgs/conteo.png', dpi = 550, width = 10, height = 5)


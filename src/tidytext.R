library(tidyverse)
library(tidytext)
library(tm)


stop <- file.path('.', 'data', 'stopes.csv') %>% 
  read.csv(sep = ";", 
           encoding = "ASCII//TRANSLIT",
           stringsAsFactors = FALSE) %>% 
  transmute(stop = as.character(X0)) 

stop <- pull(stop, stop)

# Actividades empresa_1 mención

actividades_1 <- tweets_candidatos_df %>%
  group_by(candidato) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  count(word) %>%
  ungroup() %>% 
  filter(!word %in% stop) %>%
  filter(!str_detect(word, "@\\w+ *")) %>% 
  filter(!str_detect(word, "#\\w+ *")) %>% 
  filter(!(str_length(word) == 1)) %>% 
  filter(!(str_length(word) == 2)) %>% 
  mutate(word = factor(word, levels = rev(unique(word)))) 
%>% 
  group_by(candidato) %>% 
  top_n(15)


  group_by(candidato) %>% 
  top_n(20)

ggplot(actividades_1, aes(word, n, fill = candidato)) +
  geom_col(show.legend = FALSE) +
  #labs(x = NULL, y = "tf-idf") +
  #facet_wrap(~sexo_duenio, ncol = 2, scales = "free") +
  coord_flip()

# Origen capital para iniciar con razón social_OTRO (¡nooooooooooooooooooooooooooooo!)

origen_capital_otro <- planilla %>%
  group_by(sexo_duenio) %>%
  filter(!is.na(p9otro)) %>% 
  unnest_tokens(word, p9otro) %>%
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

ggplot(origen_capital_otro, aes(word, tf_idf, fill = sexo_duenio)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~sexo_duenio, ncol = 2, scales = "free") +
  coord_flip()

# Motivo negación crédito

negacion_credito <- planilla %>%
  group_by(sexo_duenio) %>%
  filter(!is.na(p24)) %>% 
  unnest_tokens(word, p24) %>%
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

ggplot(negacion_credito, aes(word, tf_idf, fill = sexo_duenio)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~sexo_duenio, ncol = 2, scales = "free") +
  coord_flip()

# ¿Por qué no lo solicitaron al crédito? 

no_piden_prestamo <- planilla %>%
  group_by(sexo_duenio) %>%
  filter(!is.na(p28)) %>% 
  unnest_tokens(word, p28) %>%
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

ggplot(no_piden_prestamo, aes(word, tf_idf, fill = sexo_duenio)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~sexo_duenio, ncol = 2, scales = "free") +
  coord_flip()

# ¿Para qué querían utilizar crédito?

para_que <- planilla %>%
  group_by(sexo_duenio) %>%
  filter(!is.na(p29otro)) %>% 
  unnest_tokens(word, p29otro) %>%
  count(word, sort = TRUE) %>%
  ungroup() %>%
  filter(!word %in% stop) %>% 
  group_by(sexo_duenio) %>% 
  top_n(15)

ggplot(para_que, aes(reorder(word, n), n, fill = sexo_duenio)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "n") +
  facet_wrap(~sexo_duenio, ncol = 2, scales = "free") +
  coord_flip()

# Seguros otros (¡no da nada!)

seguros <- planilla %>%
  group_by(sexo_duenio) %>%
  filter(!is.na(p36)) %>% 
  unnest_tokens(word, p36) %>%
  count(word, sort = TRUE) %>%
  ungroup() %>%
  filter(!word %in% stop) %>% 
  group_by(sexo_duenio) %>% 
  top_n(15)

ggplot(seguros, aes(reorder(word, n), n, fill = sexo_duenio)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "n") +
  facet_wrap(~sexo_duenio, ncol = 2, scales = "free") +
  coord_flip()

# Área a mejorar

area_mejorar <- planilla %>%
  group_by(sexo_duenio) %>%
  filter(!is.na(p53otro)) %>% 
  unnest_tokens(word, p53otro) %>%
  count(word, sort = TRUE) %>%
  ungroup() %>%
  filter(!word %in% stop) %>% 
  group_by(sexo_duenio) %>% 
  top_n(15)

ggplot(area_mejorar, aes(reorder(word, n), n, fill = sexo_duenio)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "n") +
  facet_wrap(~sexo_duenio, ncol = 2, scales = "free") +
  coord_flip()

# Experiencias Asociativos

exp_asoc <- planilla %>%
  group_by(sexo_duenio) %>%
  filter(!is.na(p67otro)) %>% 
  unnest_tokens(word, p67otro) %>%
  count(word, sort = TRUE) %>%
  ungroup() %>%
  filter(!word %in% stop) %>% 
  group_by(sexo_duenio) %>% 
  top_n(15)

ggplot(exp_asoc, aes(reorder(word, n), n, fill = sexo_duenio)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "n") +
  facet_wrap(~sexo_duenio, ncol = 2, scales = "free") +
  coord_flip()

#Innovación

innovacion <- planilla %>%
  group_by(sexo_duenio) %>%
  filter(!is.na(p51)) %>% 
  unnest_tokens(word, p51) %>%
  count(word, sort = TRUE) %>%
  ungroup() %>%
  filter(!word %in% stop) %>% 
  group_by(sexo_duenio) %>% 
  top_n(15)

ggplot(exp_asoc, aes(reorder(word, n), n, fill = sexo_duenio)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "n") +
  facet_wrap(~sexo_duenio, ncol = 2, scales = "free") +
  coord_flip()




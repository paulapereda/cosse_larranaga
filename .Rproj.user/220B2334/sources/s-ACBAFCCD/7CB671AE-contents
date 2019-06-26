library(dplyr)
library(rtweet)
library(ggplot2)
library(twitteR)
library(syuzhet)
library(quanteda)
library(topicmodels)

# Para hacer la conexión con la api de twitter
# Keys and Token
api_key <- "9cEVJjdGIuKjaL7xYqkdhD1LI"
api_secret <- "sHBJLc1Iwpgdy9yjSls3X9DkyPfJciNxQASbB44auhgItO6GKA"
access_token <- "1140008627014635520-vY2FXYlCrH4yIeMADX1r1lelngwzWn" #
access_token_secret <- "94KCMQXD3tppzQIp4q0hhOTn9PX4r4563Q1ZT5mpl2gwn" #
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# 1. Descargar 18000 tweets de Cosse y Larrañaga

tweets_cosse <- searchTwitter("Cosse OR Carolina Cosse OR CosseCarolina",
                              n = 18000, lang = "es")
tweets_larranaga <- searchTwitter("Larrañaga OR Jorge Larrañaga OR jorgewlarranaga",
                                n = 18000, lang = "es")

# 2. Se unen las dos bases y se agrega la variable "nombre", con el nombre del precandidato

tweets_candidatos_df <- twListToDF(tweets_cosse) %>% 
  mutate(candidato = "cosse") %>% 
  rbind(twListToDF(tweets_larranaga) %>% 
          mutate(candidato = "larranaga"))

# 3. Limpiar el texto con el paquete quanteda y construir un DFM

# Se genera el corpus
tweets_candidatos <- quanteda::corpus(tweets_candidatos_df, text_field = "text")

# Abro un archivo con stopwords propias y modismos

vector <- file.path('.', 'data', 'stopes.csv') %>% 
  read.csv(sep = ";", 
           encoding = "utf-8",
           stringsAsFactors = FALSE) %>% 
  transmute(stop = as.character(X0))

# Aplico la función dfm con los argumentos para limpiar el texto
mydfm <- dfm(tweets_candidatos,
             stem = FALSE,
             tolower = TRUE,                                         # paso a minúscula todas las palabras
             remove = c(stopwords("spanish"), vector),               # saco las palabras definidas en stop
             remove_punct = TRUE,                                    # elimino puntuaciones
             remove_numbers = TRUE,                                  # elimino números
             verbose = TRUE)

# Me quedo sólo con las palabras
palabras <- featnames(mydfm)

# Saco palabras con 1 y 2 caracteres
arroba <- grep("@\\w+ *",palabras, value = TRUE)
hashtag <- grep("#\\w+ *",palabras, value = TRUE)


# Saco palabras con 1 y 2 caracteres
palabras_tam_1 <- palabras[sapply(palabras, stringr::str_length) == 1]
palabras_tam_2 <- palabras[sapply(palabras, stringr::str_length) == 2]

otraspalabras <- c(hashtag,
                   arroba,
                   "https","t.co", "rt",
                   "@cossecarolina", 
                   "carolina",
                   "cosse",
                   "carolina cosse",
                   "larrañaga",
                   "larranaga",
                   "jorge larrañaga",
                   "jorge larranaga",
                   "jorge",
                   "@jorgewlarranaga",
                   palabras_tam_1, 
                   palabras_tam_2)

mydfm <- dfm(mydfm, remove = otraspalabras)

# Nubes de palabras con quanteda
## Nubes de palabras sin desagregación
textplot_wordcloud(mydfm,
                   min.count = 10,
                   max_words = 400,
                   random.order = FALSE,
                   rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"))


# genero un nuevo data.frame por grupos según alguna variable: sexo
dfm_candidato <- dfm(mydfm, groups = "candidato")

# tiro la función para generar nube de palabras 
textplot_wordcloud(dfm_candidato, 
                   min.count = 10,
                   max_words = 400,
                   random.order = FALSE,
                   rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"),
                   comparison = T)
mydfm





# 4. Hacer un worldcloud con los principales 400 términos, desagregado para cada candidato

# Nubes de palabras con quanteda
## Nubes de palabras sin desagregación
textplot_wordcloud(mydfm,
                   min.count = 10,
                   max_words = 400,
                   random.order = FALSE,
                   rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"))

## Nubes de palabras por grupos

# genero un nuevo data.frame por grupos según alguna variable: sexo
dfm_candidato <- dfm(mydfm, groups = "candidato")

# tiro la función para generar nube de palabras 
textplot_wordcloud(dfm_candidato, 
                   min.count = 10,
                   max_words = 400,
                   random.order = FALSE,
                   rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"),
                   comparison = T)

## Topic models

dtm <- convert(mydfm, to = "topicmodels")
lda <- LDA(dtm, k = 10)
get_terms(lda,5)

##Aplico un diccionario ya existente para analizar emociones: 
#Diccionario LWIC-Spanish 

lwic <-  file.path('.', 'data', 'EmoPosNeg_SPA.rds') %>% 
  readr::read_rds()

sent_dfm <- dfm_lookup(mydfm, dictionary = lwic)

sent <- data.frame(sent_dfm)

sent$puntaje <- sent$EmoPos - sent$EmoNeg
sent$sentimiento <- ifelse(sent$puntaje < 0, "Negativo", "Positivo")
sent$sentimiento <- ifelse(sent$puntaje == 0, "Neutral", sent$sentimiento)

sent <- sent %>% 
  cbind(candidato = tweets_candidatos_df$candidato)

sent <- sent %>% 
  group_by(candidato, sentimiento) %>% 
  summarise(count = n()) %>% 
  mutate(per = round(prop.table(count)*100, 1))

ggplot(sent, aes(x = candidato,
                 y = per, 
                 fill = sentimiento)) +
  geom_bar(position="dodge",
           stat="identity") +
  scale_fill_manual(values = c("#EB594D", "#FFFAA4","#98E898"))+
  geom_text(data = sent, 
            aes(x = candidato, 
                y = per, 
                label = per),
            position = position_dodge(width = 0.9), 
            vjust = -0.25)



# Otro paquete
library(syuzhet)

## creo un score que es la difrencia entre términos positivos y negativos
tweets_candidatos_df$clean_text <- stringr::str_replace_all(tweets_candidatos_df$text, "@\\w+ *", "")
Sentiment <- get_nrc_sentiment(tweets_candidatos_df$clean_text, language = "spanish")
base_senti <- cbind(tweets_candidatos_df, Sentiment)
base_senti$puntaje <- base_senti$positive - base_senti$negative
base_senti$sentimiento <- ifelse(base_senti$puntaje < 0, "Negativo", "Positivo")
base_senti$sentimiento <- ifelse(base_senti$puntaje == 0, "Neutral",
                                 base_senti$sentimiento)

# Gráfico score sentimientos
tweets_sent <- base_senti %>% 
  group_by(candidato, sentimiento) %>% 
  summarise(count = n()) %>% 
  mutate(per = round(prop.table(count)*100,1))

ggplot(tweets_sent, aes(x=candidato, 
                        y=per, 
                        fill=sentimiento))+
  geom_bar(position="dodge", stat="identity")+
  scale_fill_manual(values = c("#EB594D", "#FFFAA4","#98E898"))+
  geom_text(data = tweets_sent, 
            aes(x = candidato, y = per, label = per),
            position=position_dodge(width=0.9), vjust=-0.25)


## En serie de tiempo
ts_tot <- base_senti %>%
  filter(created_at > "2019-01-01") %>%
  group_by(sentimiento, candidato) %>%
  rtweet::ts_plot("weeks")
ts_tot

ts_cosse <- base_senti %>%
  filter(created_at > "2019-01-01" & candidato == "Carolina Cosse") %>%
  group_by(sentimiento) %>%
  rtweet::ts_plot("weeks")
ts_cosse

ts_larranaga <- base_senti %>%
  filter(created_at > "2019-01-01" & candidato == "Jorge Larranaga") %>%
  group_by(sentimiento)%>%
  rtweet::ts_plot("weeks")
ts_larranaga


## Grafico puntajes
tweets_sent_puntaje <- base_senti %>% 
  group_by(puntaje, candidato) %>% 
  summarise(count=n()) %>% 
  mutate(per = round(prop.table(count)*100,1))

ggplot(tweets_sent_puntaje,
       aes(x = puntaje,
           y = count, 
           fill = candidato)) +
  geom_bar(position = "dodge",
           stat = "identity") +
  scale_fill_manual(values = c("#df4a4a", "#add8e6")) +
  geom_text(data = tweets_sent_puntaje, 
            aes(x = puntaje, 
                y = count, 
                label = count),
            position = position_dodge(width = 1.5), 
            vjust = -0.25)


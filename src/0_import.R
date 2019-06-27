library(rtweet)
library(twitteR)
library(syuzhet)
library(tidyverse)


# 1. Descargar 18000 tweets de Cosse y Larrañaga

tweets_cosse <- searchTwitter("Cosse OR Carolina Cosse OR CosseCarolina",
                              n = 18000, lang = "es")
tweets_larranaga <- searchTwitter("Larrañaga OR Jorge Larrañaga OR jorgewlarranaga",
                                  n = 18000, lang = "es")

# 2. Se unen las dos bases y se agrega la variable "nombre", con el nombre del precandidato

tweets_candidatos_df <- twListToDF(tweets_cosse) %>% 
  mutate(candidato = "Carolina Cosse") %>% 
  rbind(twListToDF(tweets_larranaga) %>% 
          mutate(candidato = "Jorge Larrañaga"))

# 3. Limpiar el texto con el paquete quanteda y construir un DFM

# Se genera el corpus
tweets_candidatos <- quanteda::corpus(tweets_candidatos_df, text_field = "text")

# Abro un archivo con stopwords propias y modismos

vector <- file.path('.', 'data', 'stopes.csv') %>% 
  read.csv(sep = ";", 
           encoding = "utf-8",
           stringsAsFactors = FALSE) %>% 
  transmute(stop = as.character(X........HEAD))

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
arroba <- grep("@\\w+ *", palabras, value = TRUE)
hashtag <- grep("#\\w+ *", palabras, value = TRUE)


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





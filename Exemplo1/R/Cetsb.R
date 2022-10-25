library (tidyverse)
library(lubridate)
library(forcats)
library(stringr)

cetesb <- readRDS('data/cetesb.rds')
imdb <- read.csv('data/imdb111.csv')
imdb2 <- read.csv('data/imdb.csv')
############ ator imdb


imdb2 <- imdb %>%
  separate_rows(elenco, sep = "\\, ")

#######


imdb_top <- imdb2 %>%
  group_by(elenco) %>%
  summarise(media_imdb = mean (nota_imdb),
            filmes_db = n ()
  ) %>%
  filter (filmes_db > 30) %>%
  arrange(desc(media_imdb))


imdb_top


imdb_top %>%
  ggplot()+
  geom_point(aes(x = filmes_db, y = media_imdb)) +
  coord_flip()




graf+ scale_y_discrete(breaks = seq(
  from = 6.8, to = 7, by = 0.2),
  limits = c(6.8,7)
  )

ggplot(imdb_top10, aes(x = media_imdb, y = elenco)) +
  geom_col() +
  coord_cartesian(ylim = c(6.8,10)) +
  scale_y_discrete(breaks=c(6,8,0.2))

#####################################
imdb_top10 %>%
  mutate (log = media_imdb*3.4)




imdb_top %>%
  ggplot ()+
  geom_point() (aes(x= filmes_db, y = media_imdb))






1+1

imdb_top10



imdb_top10 <- imdb_top %>%
  head(10)

jimmy1

jimmy1 %>%
  summarise(media_imdb = mean (nota_imdb),
            filmes_db = n ()
            ) %>%
  head(100)


filter (str_detect(elenco, "Cary Grant"),
imdb2

####### ator

imdb3 <- imdb %>%
  separate_rows(elenco, sep = "\\,")

1+1

jimmy<-imdb3 %>%
  filter (elenco == 'James Stewart')

imdb3 %>%
  group_by(elenco) %>%
  summarise(media_imdb = mean(nota_imdb),
            filmes_db = n ()
  ) %>%
  filter (filmes_db > 20) %>%
  arrange(desc(media_imdb))



summarise(media_lucro = mean(lucro),
          filmes_db = n()) %>%
  filter(filmes_db > 2) %>%


  group_by(generos) %>%
  summarise(lucro_medio = mean(lucro, na.rm = TRUE)) %>%

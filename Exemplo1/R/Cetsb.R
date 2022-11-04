library (tidyverse)
library(lubridate)
library(forcats)
library(stringr)
library(ggrepel)

cetesb <- readRDS('data/cetesb.rds')
imdb <- read.csv('data/imdb111.csv')
imdb2 <- read.csv('data/imdb.csv')
############ ator imdb


imdb2 <- imdb %>%
  filter (between(ano, 1900,2020)) %>%
  separate_rows(elenco, sep = "\\, ")

#######

imdb_top %>%
  filter (elenco == 'Natalie Wood')

  imdb_top %>%
    filter (elenco %in% c('Natalie Wood', 'Paul Newman'))


imdb_top <- imdb2 %>%
  group_by(elenco) %>%
  summarise(media_imdb = mean (nota_imdb),
            filmes_db = n ()
  ) %>%
  filter (filmes_db > 10) %>%
  arrange(desc(media_imdb))


imdb_top10 <- imdb_top %>%
  head(20)

imdb_top10


#imdb_top10 %>%
  #ggplot()+
  #geom_point(aes(x = media_imdb, y = filmes_db), color = 'blue', size = 2)

plot <- ggplot (imdb_top10, aes(x=media_imdb, y = filmes_db))+
  geom_point(color = 'black', size = 3)

plot +
  labs(title = 'Gráfico 1: As melhores filmografias do cinema americano ao longo da década de 1950',
       y = 'filmes',
       x = 'Nota média do IMDB',
       caption = 'Fonte: IMDB')+
  geom_label_repel(aes(label = elenco),
                   box.padding = 0.35,
                   point.padding = 0.9,
                   segment.color = 'grey50')


imdb_top10 %>%
  ggplot()+
  geom_point(aes(x = filmes_db, y = media_imdb)) +
  coord_flip()


accuracy = scales::largest_with_cents

graf+ scale_y_discrete(breaks = seq(
  from = 6.8, to = 7, by = 0.1),
  limits = c(6.8,7)
  )

ggplot(imdb_top10, aes(x = media_imdb, y = elenco)) +
  geom_col() +
  coord_cartesian(ylim = c(6.8,10)) +
  scale_y_discrete(breaks=c(6,8,0.2))

#####################################
# INDIVIDUAL

nat <- imdb2 %>%
  filter (elenco == 'James Stewart')

nat_plot <- ggplot(nat, aes(x= ano, y = nota_imdb))+
  geom_point(color = 'blue', size = 3)


nat_plot

nat_plot +
  labs (title = 'Ator')+
  geom_label_repel(aes(label = titulo),
                   box.padding = 0.35,
                   point.padding = 0.5,
                   segment.color = 'grey50'
                   )



############################ DUPLA
nat_paul <- imdb2 %>%
  filter (elenco %in% c('Paul Newman', 'Natalie Wood'))



ggplot(nat_paul, aes(x= ano, y= nota_imdb))+
  geom_line()


nba <- read.csv("http://datasets.flowingdata.com/ppg2008.csv", sep = ",")

ggplot(nba, aes(x= MIN, y = PTS)) +
  geom_point(stat = 'identity', color = "blue", size = 3)

### geom_label_repel
nbaplot +
  geom_label_repel(aes(label = Name),
                   box.padding   = 0.9,
                   point.padding = 0.5,
                   segment.color = 'grey50')



nbaa %>%
  ggplot (aes(x = reorder(Name, -PTS), y = PTS))+
  labs (x = 'Atletas da NBA', y = 'pontos por jogo')+
  geom_bar(stat = 'identity')+
  theme_light(base_size = 12)



nbaa <- nba %>%
  head(6)


imdb_top10$elenco[3]



library (tidyverse)

imdb2








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


  group_by(
    os) %>%
  summarise(lucro_medio = mean(lucro, na.rm = TRUE)) %>%



imdb2


#----------

imdb3<-imdb2 %>%
  select (titulo, ano, elenco, nota_imdb) %>%
  mutate (imdb_bom = nota_imdb > 6.5,
          imdb_baixo = nota_imdb < 6.5)
#
imdb3 %>%
  group_by(elenco) %>%
  summarise(media_imdb = mean (nota_imdb),
            filmes_db = n()) %>%
  filter (filmes_db > 1) %>%
  arrange(desc(media_imdb))

jim<-imdb3 %>%
  filter (elenco == 'James Stewart') %>%
  arrange(ano)

gra <- imdb3 %>%
  filter (elenco == 'Cary Grant') %>%
  arrange(ano)


gra_jim <- imdb3 %>%
  dplyr::filter(elenco %in% c('James Stewart', 'Cary Grant')) %>%
  arrange()






imdb2 %>%
  select (titulo, ano, elenco, nota_imdb) %>%
  filter (elenco == c('James Stewart', 'Cary Grant')) %>%
  arrange(ano)


nbaa %>%
  ggplot (aes(x = reorder(Name, -PTS), y = PTS))+
  labs (x = 'Atletas da NBA', y = 'pontos por jogo')+
  geom_bar(stat = 'identity')+
  theme_light(base_size = 12)


imdb4<-imdb3 %>%
  filter (elenco == 'James Stewart')


gg <- ggplot(mpg, aes(class))

gg <- ggplot (gra_jim, aes(elenco))

gg


gg +
  geom_bar(aes(fill = IMDB))+
  labs (x='atores', y ='filmes', title = 'Gráfico 1: Estrelas de Hollywood e carreira')+
  scale_fill_manual(values = c('578AF7','pink'),
                    labels = c('alto', 'baixo'))



g + labs (color='a')


plot +labs (colours = 'a')


p <- ggplot(mtcars, aes(mpg, wt, colour = cyl)) + geom_point()
p + labs(colour = "a")

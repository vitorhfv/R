# Filmes com maior bilheteria deflacionados


imdb4 <- imdb %>%
  select(titulo, ano, receita)


ano <- imdb4 %>%
  pull (ano) %>%
  head(50)

receita <- imdb4 %>%
  pull (receita) %>%
  head(50)


imdb_rec_50$rec_def_2016 <- adjust_for_inflation(receita, ano, 'US', to_date = 2016)

imdb_rec_50



imdb_receita <- imdb4 %>%
  arrange(desc(rec_def_2016))


imdb_rec_50 <- imdb_receita %>%
  head(50)


imdb_rec_50$rec_def_2020 <- adjust_for_inflation(receita, ano, 'US', to_date = 2020)
imdb_rec_50


imdb_receita %>%
  ggplot()+





imdb %>%
  geom_point(mapping = aes (x = orcamento, y = receita), color='blue')




imdb %>%
  filter(diretor == 'Clint Eastwood') %>%
  group_by(ano) %>%
  summarise(nota_media = mean (nota_imdb, na.rm = TRUE)) %>%
  mutate(nota_media = round(nota_media, 1)) %>%
  ggplot(aes(x = ano, y = nota_media)) +
  geom_line() +
  geom_label (aes(label = nota_media))+
  labs(title = 'Gráfico: Nota média de cineasta por década')+
  theme(text=element_text(family="Arial"))



ggsave('woody.png', plot= wooda, device=png, width = 8.5, height = 5.5, units = "in", dpi = 400)













install.packages('plotly')
library(plotly)
#################################
#
install.packages('ggthemes')
library(ggthemes)
ggthemes::theme_solid()

imdb


library(priceR)
library(ggplot2)


############################################
imdb %>%
  drop_na(receita) %>%
  head (10)

imdb
imdb<- readr::read_rds("C:\\Users\\PC\\Desktop\\Trabalhos\\imdb.rds")

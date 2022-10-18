a <- sum(1,3)
a
install.packages("tidyverse")
dados<-read.csv("dados/a.csv")
imdb <- read.csv("dados/imdb.csv")
imdb2 <- read.csv2("dados/imdb2.csv")

write.csv(imdb, "dados/dados10.csv", sep=',')
write.csv2(imdb2, 'dados/imdbbb10.csv')
write.csv(imdb, 'dados/imdb111.csv')
install.packages(('Rtools'))
install.packages("readx1")

install.packages("installr")
installr::updateR()
install.packages("tidyverse")
library(readxl)
exemplos_excel <- readxl("dados/exemplos.xlsx")
dados_excel <- read_excel("dados/exemplos.xlsx")

excel_sheets('data/exemplos.xlsx')
Cobbd <- read_excel('data/exemplos.xlsx', sheet=3)

read_excel(caminho_datasets)
Cobbd

install.packages("writexl")

library(magrittr)
x %>% f(y)

x <- c(1, 2, 3, 4)
sqrt(sum(x))

x %>% sum() %>% sqrt()

install.packages('SciViewsPackages')
library('SciViews')


airquality %>%
  na.omit() %>%
  lm(Ozone ~ Wind + Temp + Solar.R, data = .) %>%
  summary()

airquality
head(airquality)

log(19)
##### REGRESSAO
Cobbd %>%
  lm(log(Produto) ~ log(trabalho) + log(capital), data = .) %>%
  summary ()

install.packages(c("dplyr", "tidyr"))
library(dplyr)
library(tidyr)

install.packages("tibble")
library(tibble)

mcars_tibble <- as_tibble(mtcars)
mcars_tibble

mtcars$mpg

mtcars %>% head(5)
library(readr)
imdb10 <- readr::read_rds("C:\\Users\\PC\\Desktop\\Trabalhos\\imdb.rds")
imdb <- imdb10
imdb %>% head(5)

select(imdb10, titulo, ano)

glimpse(imdb10)
library (usethis)
usethis::use_blank_slate()

imdb_simples <- select(imdb10, titulo, direcao, orcamento)

imdb_contains <- select(imdb10, contains('cao'))
select(imdb, -starts_with("num"))


select(imdb10, -starts_with('num_'))

imdb_orc <- arrange(imdb10, orcamento)
imdb_orc

imdb_orc2 <- arrange(imdb10, desc(orcamento))
imdb_orc2

arrange(imdb, desc(ano), desc(orcamento))
imdb_orc3 <- arrange(imdb10, desc(ano), desc(orcamento))
imdb_orc3
# 1. Ordene os filmes em ordem crescente de ano e decrescente de receita
# e salve em um objeto chamado filmes_ordenados.

imdb100<-arrange (imdb10, ano, desc(data_lancamento))
imdb100

arrange(select(imdb10, titulo, orcamento), desc(orcamento))


arrange(select(imdb, titulo, orcamento), desc(orcamento))
arrange(select(imdb, titulo, ano), ano)

dezimdb<- imdb10 %>%
  select(titulo, ano, orcamento) %>%
  arrange(desc(orcamento))

dezimdb

dezimdb %>%
  select(titulo, ano, orcamento) %>%
  arrange(ano)

abcimdb<-arrange(imdb10, desc(ano), desc(orcamento))

select(abcimdb, titulo, ano, orcamento)


imdb_final <- imdb10 %>%
  select(titulo, nota_imdb, ano, orcamento) %>%
  arrange(desc(ano), desc(orcamento))


head(imdb_final)


imdb_final %>% filter (ano == 2000)

imdb10 %>%
  filter(nota_imdb > 7.5, (direcao %in% c("Frank Capra"))) %>%
  select(titulo, nota_imdb, ano, direcao) %>%
  arrange(desc(ano))


imdb10 %>% filter(receita - orcamento > 0)


imdb %>%
  filter(direcao %in% c("Tarantino", "Steven Spielberg"))


install.packages(("stringr"))
library("stringr")
imdb10 %>% filter(str_detect(generos, "Drama"))

library(usethis)
usethis::use_git_config( user.name='vitorhfv',
user.email = 'vitorhfv2@gmail.com'
)

usethis::use_git()
gitcreds::gitcreds_set()

usethis::use_github()
########################################
imdb10
imdb10 %>% group_by(duracao)

library(dplyr)

install.packages("remotes")
remotes::install_github("cienciadedatos/dados")

casas <- dados::casas
casas




casas %>%
  group_by(geral_qualidade) %>%
  summarise(across(
    .cols = c(lote_area, venda_valor),
    .fns = median,
    na.rm = TRUE
  ))


casas %>%
  summarise(across(.fns = n_distinct))

install.packages('basesCursoR')
#######################################

library(dplyr)
library(tidyr)
library(dados)
dados_starwars <-
dados_starwars
select(dados_starwars, 'nome', 'altura', 'massa')

dados_starwars %>%
  mutate(across(
  .cols = where (is.character),
  .fns = replace_na, replace = 'sem info'
  ))

###############################

dados_starwars %>%
  summarise(across(
    .fns = ~sum(is.na(.x))
  )) %>%
  glimpse()
########################################
starwars=dados_starwars

starwars_summarize=starwars %>%
  summarise(across(where(is.character), n_distinct)) %>%
  pivot_longer(everything(), names_to = 'coluna', values_to = 'cat_num')

starwars_summarize %>%
  arrange(desc(cat_num))

# remover nome
library(dplyr)
library(forcats)
dados_starwars %>%
  summarise(across(

  ))
imdb

library(readr)
imdb<- readr::read_rds("C:\\Users\\PC\\Desktop\\Trabalhos\\imdb.rds")
#################################
imdb_lucro_atores <- imdb %>%
  mutate(lucro = receita - orcamento) %>%
  select(titulo, lucro, starts_with("ator")) %>%
  pivot_longer(starts_with("ator"), names_to="protagonismo", values_to = "ator") %>%
  group_by(ator) %>%
  summarise(lucro_med = mean(lucro, na.rm = TRUE),
            filmes_na_db = n()
            ) %>%
  filter(filmes_na_db > 20) %>%
  arrange(desc(lucro_med))
####################################################
imdb_atores_ten <- imdb_lucro_atores[1:10, ]



imdb_atores_ten %>%
  mutate(ator = forcats::fct_reorder(ator, lucro_med))%>%
  ggplot()+
  geom_col(aes(x = ator, y = lucro_med)) +
  scale_y_continuous(labels = scales::dollar) +
  labs (x='artistas', y= 'lucro médio por filme')+
  coord_flip()

###############################
# Quais os diretores mais lucrativos

imdb_lucro = imdb %>%
  mutate(lucro = receita - orcamento) %>%
  select(titulo, ano, diretor, lucro) %>%
  filter(between(ano, 1930, 2000)) %>%
  arrange(desc(lucro))

imdb_lucro


imdb_dir_lucrat <- imdb_lucro %>%
  group_by(diretor) %>%
  summarise(media_lucro = mean(lucro),
            filmes_db = n()) %>%
  filter(filmes_db > 2) %>%
  arrange(desc(media_lucro)) %>%
  slice(1:10)

imdb_dir_lucrat %>%
  mutate(diretor = forcats::fct_reorder(diretor, media_lucro)) %>%
  ggplot() +
  geom_col(aes(x = diretor, y = media_lucro)) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Gráfico 1: Os cineastas mais lucrativos do séc. XX",
       subtitle = '(para cineastas com mais de dois filmes)',
    x = "cineastas", y = "lucro médio por filme") +
  theme(text=element_text(family="Arial"))+
  coord_flip()



#############################################
library(ggplot2)
library(forcats)
install.packages('forcats')

###############################################
Cobbd %>%
  lm(log(Produto) ~ log(trabalho) + log(capital), data = .) %>%
  summary()

library(easystats)

mod1 <- lm (log(Produto) ~ log(trabalho) + log(capital), data = Cobbd)
mod2 <- lm (Produto ~ trabalho + capital, data=Cobbd)
model_dashboard(mod1)
model_dashboard(mod2)

###########################

imdb <- read_delim(
  "https://raw.githubusercontent.com/curso-r/202010-r4ds-1/master/dados/imdb2.csv",
 ";",
  escape_double = FALSE,
  trim_ws = TRUE
 )

#
imdb_lucro

####################

personagem_mais_baixo <- dplyr::starwars %>%
  arrange(desc(height)) %>%
  slice(1)

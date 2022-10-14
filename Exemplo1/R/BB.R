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

excel_sheets('dados/exemplos.xlsx')
Cobbd <- read_excel('dados/exemplos.xlsx', sheet=3)

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

imdb10 <- readr::read_rds("dados/imdb.rds")
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
